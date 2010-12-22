(load "../shop2/shop2")

(defun sqr (x)
  (* x x))

; Distancia entre dos puntos, dados como un par de listas:
(defun distance (x y)
  (let ((x0 (car x))
        (y0 (cadr x))
        (x1 (car y))
        (y1 (cadr y)))
    (sqrt (+ (sqr (- x1 x0)) (sqr (- y1 y0))))))

(defdomain garaje 
           (
            ; MOVE
            ; this models the cost of travelling from a point to another one
            (:operator (!move ?who ?coords)
                       (:sort-by ?timestamp (at ?who ?origin ?timestamp))
                       ((at ?who ?origin ?timestamp))
                       ((at ?who ?origin (eval (+ ?timestamp (distance '?origin '?coords)))))
                       (distance '?origin '?coords)
            )

            ; ROTATE
            ; this models the cost of doing an in-place rotation. Since we
            ; can't know the starting angle, we can't model the cost.
           (:operator (!rotate ?who)
                      ((at ?who ?coords ?timestamp))
                      ((at ?who ?coords ?timestamp))
                      ((at ?who ?coords (eval (+ ?timestamp 1)))))
            
            ; VISIT
            ; This method models our desire to place any agent in a given lieu
            (:method (visit ?coords)
                     ; Preferred, something already there
                     rest
                     ((at ?who ?coords ?timestamp))
                     ()
                     ; Move someone
                     move
                     ((at ?who ?origin ?timestamp))
                     ((!move ?who ?coords)))
))

(defproblem test garaje
            ((at Ari (0 0) 0) (at Ben (15 15) 0) (at Ced (30 30) 0) (at Dan (50 10) 0))
            (:unordered (visit (10 10)) (visit (50 50)) (visit (20 20)) (visit (30 30)) (visit (25 12)) (visit (135 1)))
)

(shop-trace ':states)
(find-plans 'test :which :all :verbose :plans)
