(include "ds1054z.scm")
(import chicken.port)

(when (terminal-port? (current-output-port))
  (error "in terminal? refusing to dump raw PNG. try
  ds1054z-screenshot > screenshot.png # or
  ds1054z-screenshot | feh -"))

(display (screenshot/png))
