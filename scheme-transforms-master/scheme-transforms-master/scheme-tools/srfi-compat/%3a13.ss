#!r6rs

;; In order to be compatible with (rnrs), this does not export:
;; string-upcase, string-copy, string-for-each, string-hash, string-downcase, string-titlecase, string->list

(library

 (scheme-tools srfi-compat :13)

 (export string-map string-map!
         string-fold
         string-unfold
         string-fold-right
         string-unfold-right
         string-tabulate         
         string-for-each-index
         string-every
         string-any         
         string-hash-ci
         string-compare
         string-compare-ci
         string=
         string<
         string>
         string<=
         string>=
         string<>
         string-ci=
         string-ci<
         string-ci>
         string-ci<=
         string-ci>=
         string-ci<>                          
         string-downcase!
         string-upcase!
         string-titlecase!
         string-take
         string-take-right
         string-drop
         string-drop-right
         string-pad
         string-pad-right
         string-trim
         string-trim-right
         string-trim-both
         string-filter
         string-delete
         string-index
         string-index-right
         string-skip
         string-skip-right
         string-count
         string-prefix-length
         string-prefix-length-ci
         string-suffix-length
         string-suffix-length-ci
         string-prefix?
         string-prefix-ci?
         string-suffix?
         string-suffix-ci?
         string-contains
         string-contains-ci
         string-copy!
         substring/shared
         string-reverse
         string-reverse!
         reverse-list->string
         string-concatenate
         string-concatenate/shared
         string-concatenate-reverse
         string-concatenate-reverse/shared
         string-append/shared
         xsubstring
         string-xcopy!
         string-null?
         string-join
         string-tokenize
         string-replace
         ;; R5RS extended:         
         string-fill!
         ;; R5RS re-exports:
         string?
         make-string
         string-length
         string-ref
         string-set!
         string
         string-append
         list->string
         ;; Low-level routines:
         ;; make-kmp-restart-vector
         ;; string-kmp-partial-search
         ;; kmp-step
         ;; string-parse-start+end
         ;; string-parse-final-start+end
         ;; let-string-start+end
         ;; check-substring-spec
         ;; substring-spec-ok?
         )

 (import (srfi :13 strings))
 
 )

