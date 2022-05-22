;;     [PG]: https://www.batronix.com/pdf/Rigol/ProgrammingGuide/MSO1000Z_DS1000Z_ProgrammingGuide_EN.pdf
(import fmt srfi-1 chicken.io chicken.tcp chicken.string chicken.blob)


(define (slurp ip)
  (let loop ((s ""))
    (if (char-ready? ip)
        (loop (conc s (read-char ip)))
        s)))

(define-values (ip op) (tcp-connect "10.0.0.160" 5555))

(define (cmd line)
  (display (conc line "\n") op))

(define (cmd? line) ;; careful!
  (cmd line)
  (read-line ip))

(define (chunk/asc start len)
  (cmd ":WAV:FORM ASC")
  (define stop (+ start len -1))
  (cmd (conc ":WAV:STAR " start))
  (cmd (conc ":WAV:STOP " stop))
  (unless (equal? start (string->number (cmd? ":WAV:STAR?"))) (error "end at " start))
  (unless (equal? stop  (string->number (cmd? ":WAV:STOP?"))) (error "end at " stop))

  (cmd ":WAV:DATA?")
  (let* ((line (read-line ip))
         (headless (substring line 11))
         (parts (string-split headless ",")))
    (map string->number parts)))

(define yor  (string->number (cmd? ":WAV:YOR?")))
(define yref (string->number (cmd? ":WAV:YREF?")))
(define yinc (string->number (cmd? ":WAV:YINC?")))
;; see [PG] page 221
(define (raw->volt byte)
  (* (- byte yor yref) yinc))

;; faster form of chunk/asc, but with slightly lower precision
(define (chunk/byte start len)
  (cmd ":WAV:FORM BYTE")
  (define stop (+ start len -1))
  (cmd (conc ":WAV:STAR " start))
  (cmd (conc ":WAV:STOP " stop))
  (unless (equal? start (string->number (cmd? ":WAV:STAR?"))) (error "end at " start))
  (unless (equal? stop  (string->number (cmd? ":WAV:STOP?"))) (error "end at " stop))

  (cmd ":WAV:DATA?")
  (let* ((line (read-line ip))
         (headless (substring line 11)))
    (map (lambda (char) (- (raw->volt (char->integer char)) 0.0000025000)) (string->list headless))))

(define (chan-disp? n)
  (unless (and (> n 0) (<= n 4)) (error "expecting channel 1-4, no?" n))
  (if (= 1 (string->number (cmd? (conc ":CHAN" n ":DISP?"))))
      #t
      #f))

(define (chan-source n)
  (unless (and (> n 0) (<= n 4)) (error "expecting channel 1-4, no?" n))
  (cmd (conc ":WAV:SOUR CHAN" n))

  (set! yor  (string->number (cmd? ":WAV:YOR?")))
  (set! yref (string->number (cmd? ":WAV:YREF?")))
  (set! yinc (string->number (cmd? ":WAV:YINC?"))))

;; "#9000056185\xEF\xBF\xBDPNG…"
(define (read-tmc)
  (let* ((hash (read-string 1 ip))
         (digits (string->number (read-string 1 ip)))
         (length (string->number (read-string digits ip))))
    (read-string length ip)))

(define (screenshot/png)
  (cmd ":DISP:DATA? ON,OFF,PNG")
  (read-tmc))


