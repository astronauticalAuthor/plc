; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014

; (load "chez-init.ss") 
(load "C:/Users/Max Kelly/Documents/Classes/CSSE304 PLC/A13-multiple-files/plc/chez-init.ss")

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "C:/Users/Max Kelly/Documents/Classes/CSSE304 PLC/A13-multiple-files/plc/datatypes.ss")
    (load "C:/Users/Max Kelly/Documents/Classes/CSSE304 PLC/A13-multiple-files/plc/parse.ss")
    (load "C:/Users/Max Kelly/Documents/Classes/CSSE304 PLC/A13-multiple-files/plc/env.ss")
    (load "C:/Users/Max Kelly/Documents/Classes/CSSE304 PLC/A13-multiple-files/plc/interpreter.ss")))

(load-all)

(define l load-all) ; even easier!
