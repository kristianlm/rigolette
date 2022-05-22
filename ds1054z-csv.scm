(include "ds1054z.scm")

;; bigger page size make things much faster
(define page 250000) ;; max page size in BYTE mode
;; (define page 100) ;; for testing

(cmd ":STOP") ;; must be stopped for that to work --⹁
(cmd ":WAV:MODE RAW") ;; get access to all samples (internal RAM)

(define channels (filter chan-disp? '(1 2 3 4)))

 ;; header line
(fmt #t (fmt-join (cut cat "CH" <>) channels ",") nl)

;; TODO: find the right number of samples upfront, so we get the last
;; page too.

(let loop ((start 1))

  (define chunks ;; ( (1 1 0 ) ... ( 20 20 21 ...))
    (let next ((channels channels)
               (chunks '()))
      (if (pair? channels)
          (begin
            (chan-source (car channels))
            (next (cdr channels)
                  (cons 
                   (chunk/byte start page)
                   chunks)))
          (reverse chunks))))
  
  (define rows (apply zip chunks))
  
  (for-each (lambda (row) (fmt #t (fmt-join dsp row ",") nl)) rows)
  (flush-output)

  (loop (+ start page)))
