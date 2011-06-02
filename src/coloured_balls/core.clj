(ns coloured-balls.core
  ;; sk-change: Changed :use to :require so that I can easily see
  ;; when rosado.processing is used.  (In production code, I think I
  ;; would be tempted to use the :as option to :require.)
  (:require [rosado.processing]
            [rosado.processing.applet])
  (:gen-class))

;; here's a function which will be called by Processing's (PApplet)
;; draw method every frame. Place your code here. If you eval it
;; interactively, you can redefine it while the applet is running and
;; see effects immediately

(defrecord ball
    ;; sk-change:
    ;; - Changed defstruct to defrecord.
    ;;   - defstruct is old.  defrecord was new in Clojure 1.2.
    ;;   - The book "Practical Clojure" says (on page 185) "prefer
    ;;     defrecord to defstruct in all cases".
    ;;   - At the time of writing this I don't have an internet
    ;;     connection, so I can't find an on-line reference.
    ;; - Added id (a string) to ball.  This is displayed in the middle
    ;;   of the graphic for the ball.  I found this useful when
    ;;   debugging some changes.
    ;; - Changed radius to diameter.  (The original was using the wrong
    ;;   word.)
    [id x y vx vy red blue green diameter])

(defn draw-ball [ball]
  (rosado.processing/fill (:red ball) (:green ball) (:blue ball))
  (rosado.processing/ellipse (:x ball) (:y ball) (:diameter ball) (:diameter ball))
  ;; sk-change: Draw the id of the ball at its centre.
  (rosado.processing/fill 0 0 0)
  (rosado.processing/string->text (:id ball) (:x ball) (:y ball)))

(def window-width 200)  ; sk-change: Added this.
(def window-height 600) ; sk-change: Added this.
;; sk-change: Make the window tall and thin so I can display it to the
;; side of my editor window and easily see the effect of changes when
;; I evaluate them.

(defn make-ball [id]
  ;; sk-change:
  ;; - Takes id as an argument.
  ;; - Added wh and random-velocity-component.
  ;; - Take account of ball's radius when setting initial position, so
  ;;   ball is completely within the window.
  (let [wh (rand-int 70)]
    (letfn [(random-velocity-component
             []
             (- (* 2 (rand-int 5)) 5))]
      (ball. id
             (+ wh (rand-int (- window-width (* 2 wh))))
             (+ wh (rand-int (- window-height (* 2 wh))))
             (random-velocity-component)
             (random-velocity-component)
             (rand-int 256)
             (rand-int 256)
             (rand-int 256)
             wh))))

(def n-balls 10)

;; sk-change: make-ball now takes an argument, so re-arranged the code
;; that calls it.
(def ball-state (atom (map (comp make-ball str) (range n-balls))))

(defn move [ball]
  ;; sk-change: Changed nested calls of update-in to a single call of
  ;; assoc.  I think this is easier to read.  YMMV.
  (assoc ball
    :x (+ (:x ball) (:vx ball))
    :y (+ (:y ball) (:vy ball))))


;; sk-change: Added wall-bounciness.  (Just playing.)
;; A value less than 1 causes balls to slow down when bouncing off a
;; wall.
;; A value greater than 1 causes balls to speed up when bouncing off a
;; wall.
;; A value of 1 causes no change to the speed.

(def wall-bounciness 1)

(defn apply-wall-bounciness [x]
  (let [signum (cond (< x 0) -1
                     (= x 0)  0
                     :else    1)
        abs (Math/abs x)]
    (* signum
       (if (< wall-bounciness 1)
         (max 1 (* abs wall-bounciness))
         (min 20 (* abs wall-bounciness))))))

(defn maybe-collide-with-wall [ball] ; sk-change: Renamed. (Was collide.)
  ;; sk-change:
  ;; - I think there was a mini-bug in the original: if a ball hit
  ;;   two walls at the same time, it would bounce off only one wall
  ;;   on this iteration and would bounce off the other wall on the
  ;;   next iteration, thereby going into one of the walls a little
  ;;   bit.
  ;; - Other than fixing the mini-bug, I've perhaps made this worse
  ;;   than the original -- it's certainly considerably more lines of
  ;;   code, and it's not very clear.
  (let [radius (/ (:diameter ball) 2)
        make-adjust-fun (fn [position-key
                             velocity-key
                             distance-to-wall-fun
                             new-position]
                          (fn [ball]
                            (if (< (distance-to-wall-fun ball) radius)
                              (assoc ball
                                velocity-key (apply-wall-bounciness
                                              (- (velocity-key ball)))
                                position-key new-position)
                              ball)))
        adjust-left (make-adjust-fun :x
                                     :vx
                                     :x
                                     radius)
        adjust-right (make-adjust-fun :x
                                      :vx
                                      #(- window-width (:x %))
                                      (- window-width radius))
        adjust-top (make-adjust-fun :y
                                    :vy
                                    :y
                                    radius)
        adjust-bottom (make-adjust-fun :y
                                       :vy
                                       #(- window-height (:y %))
                                       (- window-height radius))]
    (adjust-left (adjust-right (adjust-top (adjust-bottom ball))))))

(defn balls-colliding? [b1 b2] ; sk-change: Renamed. (Was collides?.)
  (and (not= b1 b2) ; sk-change: Added this test so it does not have
                    ; to be done at the call site.
       (< (:diameter b1) (:diameter b2))
       (let [dx (- (:x b1) (:x b2))
             dy (- (:y b1) (:y b2))]
         (< (Math/sqrt (+ (* dx dx) (* dy dy)))
            (/ (+ (:diameter b1) (:diameter b2)) 2)))))

(defn vortho [[x y]]
  [(- y) x])

(defn vadd [[u v] [x y]]
  [(+ u x) (+ v y)])

(defn vsub [[u v] [x y]]
  [(- u x) (- v y)])

(defn vlen [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn vunit [[x y]]
  [(/ x (vlen [x y]))
   (/ y (vlen [x y]))])

(defn vmul [a [x y]]
  [(* a x) (* a y)])

(defn reflect [ball other-ball]
  (let [pb [(:x ball) (:y ball)]
        po [(:x other-ball) (:y other-ball)]
        velocity [(:vx ball) (:vy ball)]
        connector (vunit (vsub po pb))
        new-velocity (vmul (- (vlen velocity))
                           (vunit connector))]
    (assoc ball :vx (first new-velocity) :vy (second new-velocity))))

(defn colliding? [ball balls]
  (some (partial balls-colliding? ball)
        balls))

(defn update-for-collisions [balls] ; Renamed (was mutual-collisions).
  ;; (map (fn [ball]
  ;;        (let [other-ball ; sk-change: Renamed (was crash).
  ;;              ;; sk-change:
  ;;              ;; - re-wrote the following form in a way that I think
  ;;              ;;   is easier to read
  ;;              ;; - no need to test for ball and other-ball being the
  ;;              ;;   same -- that's now dealt with in balls-colliding? .
  ;;              (first (filter (partial balls-colliding? ball)
  ;;                             balls))]
  ;;          (if (not (nil? other-ball))
  ;;            (reflect ball other-ball)
  ;;            ball))))
  (remove (fn [b] (colliding? b balls))
          balls))


(defn update-state-and-draw! ; sk-change: Renamed (was draw).
  "Example usage of with-translation and with-rotation."
  []
  ;; sk-change: Replaced two swap!s with a single swap!.  I think that
  ;; better reflects what's happening.
  (swap! ball-state (comp (partial map move)
                          update-for-collisions
                          (partial map maybe-collide-with-wall)))
  (rosado.processing/background 226)
  (doall
   (map draw-ball @ball-state)))

(defn setup []
  "Runs once."
  (rosado.processing/smooth)
  (rosado.processing/no-stroke)
  (rosado.processing/fill 226)
  (rosado.processing/framerate 20))

;; Now we just need to define an applet:

(rosado.processing.applet/defapplet balls :title "Coloured balls"
  :setup setup
  :draw update-state-and-draw!
  :size [window-width window-height])

(rosado.processing.applet/run balls)
