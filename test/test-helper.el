(when (require 'undercover nil t)
  (undercover "*.el" (:send-report nil)))

(require 'scala-quick-import)
