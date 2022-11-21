;; (include "rigolette.scm")
(import rigolette
        (only chicken.string conc)
        (only chicken.process-context get-environment-variable)
        (only chicken.file file-exists?)
        (only chicken.port terminal-port? with-output-to-port with-output-to-string
              make-bidirectional-port)
        (only chicken.process-context command-line-arguments)
        (only chicken.io read-list)
        (only chicken.pretty-print pretty-print)
        (only chicken.pathname make-pathname)
        (only chicken.string string-split)
        (only chicken.repl repl)
        (only chicken.irregex irregex-replace/all)
        srfi-1)


;; cli <==> eval friendliness. let's eval reference channels etc easily
(define NORM "NORM")
(define C1 "CHAN1")
(define C2 "CHAN2")
(define C3 "CHAN3")
(define C4 "CHAN4")
(define T  'time)

(define (die msg code)
  (with-output-to-port (current-error-port)
    (lambda () (print msg)))
  (exit code))

(define (./rigolette.scm)
  (or (get-environment-variable "RIGOLETTE_CONFIG")
      (make-pathname
       (or (get-environment-variable "XDG_CONFIG_HOME")
           (make-pathname (get-environment-variable "HOME") ".config"))
       "rigolette.scm")))

(define example-config-command
  (conc "    echo '"
        "(define (connect) (tcp-connect \"10.0.0.160\" 5555))"
        "' >" (./rigolette.scm) "\n"
        "    ;; where 10.0.0.160 is the IP of your Rigol Oscilloscope.\n"))

(define (connect) (error (conc "`connect` must be defined in your config. eg: "
                               example-config-command)))

(eval `(import ;;rigolette
               chicken.tcp chicken.string chicken.port chicken.io
               (only chicken.repl quit)
               srfi-1))

(if (file-exists? (./rigolette.scm))
    (let ((config `(begin ,@(with-input-from-file (./rigolette.scm) read-list))))
      (info (with-output-to-string (cut pretty-print config)))
      (eval config))
    (die
     (print "error: could not find " (./rigolette.scm) "\n"
            "here's a suggestion:\n"
            "" example-config-command "\n")
     10))

(info "running: " connect)

(current-rigol-port (call-with-values connect make-bidirectional-port))

;; (cmd ":STOP") ;; must be stopped for that to work --⹁
;; (cmd ":WAV:MODE SCREEN")
;; (cmd ":WAV:MODE RAW") ;; get access to all samples (internal RAM)


;; TODO: find the right number of samples upfront, so we get the last
;; page too.

(define (screenshot!)
  (when (terminal-port? (current-output-port))
    (die "in terminal? refusing to dump raw PNG. try
  rigolette screenshot > screenshot.png # or
  rigolette screenshot | feh -"
         11))

  (display (screenshot/png)))

(define channels #f)

(define (csv)
  ;; TODO: add timestamp column
  ;; CSV header line
  (define stop (wav-stop-limit))
  (warn "csv: downloading from " channels " with " stop " datapoints each")
  (for-each display (intersperse channels ","))
  (newline)
  (wav-points-fold
   (lambda (row _) (for-each display (intersperse row ", ")) (newline))
   '_
   channels
   #:stop stop))

(define (status)
  (print (cmd? "*IDN?")))

(let loop ((cla (command-line-arguments))
           (exe '()))
  (if (pair? cla)
      (cond  ((equal? (car cla) "-v")
              (set! info log-stderr)
              (loop (cdr cla) exe))

             ((equal? (car cla) "-m")
              (set! (wav-mode) (cadr cla))
              (loop (cddr cla) exe))

             ((equal? (car cla) "-c")

              (let* ((chans (irregex-replace/all `"," (cadr cla) " ")) ;; "1,C2" => "1 C2"
                     (chans (with-input-from-string chans read-list)) ;; '(1 C2)
                     (chans (map ;; '("CHAN1" "CHAN2") ;; (C2 is a variable)
                             (o (lambda (x) (if (number? x) (conc "CHAN" x) x)) eval)
                             chans)))
                (set! channels chans))
              (info "using channels: " (with-output-to-string (lambda () (write channels))))
              (loop (cddr cla) exe))

             ((equal? (car cla) "csv")
              (unless channels
                (set! channels (filter chan-disp? '("CHAN1" "CHAN2" "CHAN3" "CHAN4"))))
              (loop (cdr cla) (cons csv exe)))

             ((equal? (car cla) "run")        (loop (cdr cla) (cons run! exe)))
             ((equal? (car cla) "stop")       (loop (cdr cla) (cons stop! exe)))
             ((equal? (car cla) "screenshot") (loop (cdr cla) (cons screenshot! exe)))
             ((equal? (car cla) "status")     (loop (cdr cla) (cons status exe)))
             ((equal? (car cla) "repl")       (loop (cdr cla) (cons repl exe)))
             (else (die (conc "unrecognized argument: " (car cla)) 100)))
      (if (pair? exe)
          (for-each (lambda (e) (e)) (reverse exe))
          (die
           (print "usage: [-v] [-m RAW/NORM/MAX] [-c CHAN1,CHAN2,…] <command>
where <command> is one of:
    run  - start the scope
    stop - stop the scope
    csv - print waveform points as CSV data.
    screenshot - capture scope framebuffer and print to stdout as PNG
    status - Retrieve serial number from target
") 101))))
