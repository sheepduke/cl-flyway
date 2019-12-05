(defpackage cl-flyway
  (:nicknames flyway)
  (:use #:cl #:alexandria)
  (:import-from #:mito
                #:deftable
                #:insert-dao)
  (:import-from #:sxql
                #:yield
                #:select
                #:from
                #:where
                #:create-table
                #:insert-into
                #:set=))
