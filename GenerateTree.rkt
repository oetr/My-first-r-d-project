;; Open a data set and read some specified values from it

;;; Open and read the data set


;; some test data set
(define file "../Data/ValueSim-2011-3-7-13-49-52.txt")

(define port (open-input-file file #:mode 'text))
(close-input-port port)

(file-position port)
(read-char port)
(read-line port)

(define document (port->lines port))
(length document)
(last document)


