;;     [PG]: https://www.batronix.com/pdf/Rigol/ProgrammingGuide/MSO1000Z_DS1000Z_ProgrammingGuide_EN.pdf
(import chicken.io chicken.tcp chicken.string chicken.blob
        (only chicken.port with-output-to-port with-output-to-string)
        srfi-1)

;; debug logs. you can (set! info (lambda () ...)) to customize.
(define (log-stderr . args)
  (with-output-to-port (current-error-port)
    (lambda () (apply print args))))

;; (set! info log-stderr) ;; to customize
(define info (lambda _ #f))
(define warn log-stderr)

(define current-rigol-port (make-parameter #f))

(define (cmd line #!optional quiet)
  (unless quiet (info "cmd " line))
  (display (conc line "\n") (current-rigol-port)))

(define (cmd? line) ;; careful!
  (cmd line #t)
  (let ((l (read-line (current-rigol-port))))
    (info "  ? " line " => " l)
    l))

;; lime cmd, but double-check answer afterwards
(define (cmd!? c value)
  (cmd (conc c " " value))
  (unless (equal? (cmd? (conc c "?")) (conc value))
    (error (conc "failed setting " c " to " (with-output-to-string (lambda () (write value)))))))

;; read binary data chunk.
;; (call-with-input-string "#9000000001\x00\n" read-tmc) => "\x00"
(define (read-tmc #!optional (ip (current-rigol-port)))
  (let* ((hash (read-string 1 ip))
         (digits (string->number (read-string 1 ip)))
         (length (string->number (read-string digits ip))))
    (unless (equal? hash "#") (error "expected #, got " hash))
    (info "reading " length " tmc bytes")
    (let ((result (read-string length ip))
          (trail (read-char ip)))
      (unless (equal? trail #\newline) (error "expected newline, got" trail))
      result)))

(define (wav-range start len)
  
  (define stop (+ start len -1))
  
  (cmd (conc ":WAV:STAR " start))
  (cmd (conc ":WAV:STOP " stop))
  (let ((stop0 (cmd? ":WAV:STOP?")))
    (unless (= stop (string->number stop0))
      (error "problematic stop (scope running?)" (conc stop "≠" stop0)))))

;; issuing a :WAV:STOP command that's outside of the available range
;; has no effect. exploiting this fact to find the highest possible
;; valid position. I wish MDEP would always return a numeric value,
;; not "AUTO" most of the time.
(define (acq-mdep-discover-hack
         #!key (tries
                '(24000000
                  12000000
                  6000000
                  3000000
                  1200000
                  600000
                  300000
                  120000
                  60000
                  30000
                  12000
                  6000
                  3000
                  1200)))
  (let loop ((tries tries))
    (if (pair? tries)
        (let ((stop (car tries)))
          (cmd (conc ":WAV:STOP " stop))
          (if (= stop (string->number (cmd? ":WAV:STOP?")))
              stop
              (loop (cdr tries))))
        (error (conc "MDEP is AUTO, and could not determine size. "
                     "consider e.g. (set! (acq-mdep) 3000)")))))

(define acq-mdep
  (getter-with-setter
   (lambda ()
     (let ((mdep (cmd? ":ACQ:MDEP?")))
       (if (equal? "AUTO" mdep)
           (acq-mdep-discover-hack)
           (string->number mdep))))
   (lambda (v)
     (cmd (conc ":ACQ:MDEP " v)))))

;; TODO: struggle even more to find out how to discover number of
;; points in DATA.
(define (wav-stop-limit)
  (let ((wm (string->symbol (wav-mode))))
    (case wm ;; [PG] 2-225
     ((NORM) 1200)
     ((MAX) (error "wav-stop-limit for MAX: TODO"))
     ((RAW) (acq-mdep))
     (else (error "wav-stop-limit: unknown wav-mode" wm)))))

(define (chunk/asc start len)
  (cmd ":WAV:FORM ASC")
  (wav-range start len)
  (cmd ":WAV:DATA?")
  (let* ((data (read-tmc))
         (parts (string-split data ",")))
    (map string->number parts)))

(define (run!) (cmd ":RUN"))
(define (stop!) (cmd ":STOP"))

(define wav-mode
  (getter-with-setter
   (lambda () (cmd? ":WAV:MODE?"))
   (lambda (mode) (cmd!? ":WAV:MODE" mode))))

(define wav-format
  (getter-with-setter
   (lambda () (cmd? ":WAV:FORM?"))
   (lambda (mode) (cmd (conc ":WAV:FORM " mode)))))

(define (wav-yor)  (string->number (cmd? ":WAV:YOR?")))
(define (wav-yref) (string->number (cmd? ":WAV:YREF?")))
(define (wav-yinc) (string->number (cmd? ":WAV:YINC?")))

;; convert blob to list of numbers, where 1 byte turns into 1 number
;; with the relevant scaling. yref and friends change between each page.
;; 
;; (bytes->wavepoints "abc" 0 0 1) => (23.5066391 23.8080063 24.1093735)
(define (bytes->wavepoints str yor yref yinc)
  ;; see [PG] page 221
  (define (raw->volt byte)
    (* (- byte yor yref) yinc))
  
  (let loop ((i (- (string-length str) 1))
             (result '()))
    (if (>= i 0)
        (let ((char (string-ref str i)))
          (loop (- i 1) ;;    makes them equal to ascii version ↴
                (cons (- (raw->volt (char->integer char)) 0.0000025000) result)))
        result)))

;; faster form of chunk/asc, but with slightly lower precision
(define (chunk/byte start len #!optional
                    (yor (wav-yor))
                    (yref (wav-yref))
                    (yinc (wav-yinc)))
  (wav-range start len)
  (cmd ":WAV:DATA?")
  (let* ((data (read-tmc)))
    (bytes->wavepoints data yor yref yinc)))

(define chan-disp?
  (getter-with-setter
   (lambda (chan) (= 1 (string->number (cmd? (conc ":" chan ":DISP?")))))
   (lambda (chan disp?) (cmd  (conc ":" chan ":DISP " (if disp? 1 0))))))

(define chan-unit
  (getter-with-setter
   (lambda (ch)      (cmd? (conc ":" ch ":UNIT?")))
   (lambda (ch unit) (cmd  (conc ":" ch ":UNIT " unit)))))

(define chan-source
  (getter-with-setter
   (lambda ()   (cmd? ":WAV:SOUR?"))
   (lambda (ch) (cmd!? ":WAV:SOUR" ch))))

(define (screenshot/png)
  (cmd ":DISP:DATA? ON,OFF,PNG")
  (read-tmc))

;; bigger page size make things much faster
(define (default-pagesize)
  (case (string->symbol (wav-format)) ;; [PG] 2-219
    ((BYTE) 250000)
    ((WORD) 125000)
    ((ASC)  15625)
    (else (error "cannot determine default-pagesize, unknown wav-format: " (wav-format)))))

;; good idea to set wav-format before doing this.
;; eg. ((1 100) (101 100)) ;; where you have 200 points total.
;; STAR is inclusive, STOP is not.
(define (wav-pages #!key
                   (start 1)
                   pagesize
                   stop)
  (unless stop (set! stop (wav-stop-limit))) ;; allow passing #:stop #f
  (unless pagesize (set! pagesize (min stop (default-pagesize))))
  (let loop ((start start)
             (pages '()))
    (if (>= start stop)
        (reverse pages)
        (loop (+ start pagesize)
              (cons (list start pagesize) pages)))))

;; download 1 page worth of datapoints for a list of channels. note
;; that pagesize must obey the device's size limits. returns a list of
;; rows like this: ((ch1-1 ch2-1) (ch1-2 ch2-2) (ch1-3 ch2-3) …)
(define (wav-page-points channels
                         start
                         pagesize
                         #!key
                         fetch)
  (unless fetch
    (set! (wav-format) "BYTE")
    (set! fetch (cut chunk/byte <> <> (wav-yor) (wav-yref) (wav-yinc))))
  (let loop ((channels channels)
             (chunks '()))
    (if (pair? channels)
        (begin
          (set! (chan-source) (car channels))
          (loop (cdr channels)
                (cons 
                 (fetch start pagesize)
                 chunks)))
        (apply zip (reverse chunks)))))

(define (wav-points-fold
         proc initial
         channels
         #!key
         (start 1)
         stop
         pagesize
         fetch)
  (unless (list? channels) (error "`channels': expecting list" channels
                                  'wav-points-fold))
  
  (let loop/page ((pages (wav-pages #:start start #:pagesize pagesize #:stop stop))
                  (result initial))
    (if (pair? pages)
        (let* ((page (car pages))
               (start (car page))
               (pagesize (cadr page))
               (rows (wav-page-points channels start pagesize #:fetch fetch)))
          (let loop/row ((rows rows)
                         (result result))
            (if (pair? rows)
                (loop/row (cdr rows)
                          (proc (car rows) result))
                (loop/page (cdr pages) result))))
        result)))
