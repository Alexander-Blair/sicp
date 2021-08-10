; 5.31

(f 'x 'y)
; With optimisation, all the saves could be done away with

((f) 'x 'y)
; With optimisation, all the saves could be done away with

(f (g 'x) y)
; You would need to save the proc, the env and the arglist before executing (g 'x),
; since y depends on the env.

(f (g 'x) y)
; You would need to save the proc and the arglist only before executing (g 'x)

; 5.32

; 1. See commit 514299f

; 2. The compiler would still have an advantage in that expressions would be analysed once
;    (at compile time). The interpreter would reevaluate the same expression if it cropped
;    up more than once.

; 5.33

; There is one subtle difference: in the original, n is accumulated into the argument
; list before we recursively call factorial, so we need to save and restore the argl
; register, but we do not need to save and restore env. It's the opposite in the new
; version of the function. The difference in efficiency seems negligible - it's possible
; that argl is a less expensive register to save in comparison to env, which would
; favour the original implementation.

; 5.34
