(require
  hy-functional.core *)

(eval-and-compile
  (import
    hy-functional.core *))

;; Reducing Function:
;; - acc->acc :: accept unreduced acc, return unreduced acc.
;; - (acc,input)->acc :: accept unreduced acc and input, return maybe reduced acc.

(defclass [(dataclasses.dataclass :slots True)] reduced []
  #^ typing.Any data

  (defn [classmethod] cast [cls obj]
    (if (isinstance obj cls)
      obj
      (cls obj))))

(defn unreduced [x]
  (if-not (isinstance x reduced)
    x
    x.data))

(defn completing [rf [cf identity]]
  (farity
    ([acc] (cf acc))
    ([acc input] (rf acc input))))

(defn reduce-step [rf init iterable]
  (loop [s (seq.cast iterable) acc init]
    (if (or s.empty? (isinstance acc reduced))
      acc
      (recur s.rest (rf acc s.first)))))

(defn reduce [rf init iterable]
  (-> (reduce-step rf init iterable) unreduced rf))

(defn transduce [xform rf init iterable]
  (reduce (xform f) init iterable))

(defn eduction [xform iterable]
  (let [xconj (xform (completing conj))]
    (loop [s (seq.cast iterable) acc (list)]
      (if (isinstance acc reduced)
        (yield :from (xconj acc.data))
        (if acc
          (do
            (yield :from acc)
            (recur s (list)))
          (if s.empty?
            (yield :from (xconj acc))
            (recur s.rest (xconj acc s.first))))))))

;;; transducers

(defmacro make-transducer [#* opts]
  (let [opts (dict (partition 2 opts))]
    `(fn [rf]
       (fn [#* args]
         ~@(let [nonlocals (.get opts ':nonlocals)]
             `((nonlocal ~@nonlocals)))
         (match args
           #(acc) ~(.get opts ':comp '(rf acc))
           #(acc input) ~(.get opts ':step '(rf acc input))
           _ (raise TypeError))))))

(defn halt-when [pred [ef identity]]
  (let [halt? False]
    (make-transducer
      :nonlocals [halt?]
      :comp (if halt?
              acc
              (rf cc))
      :step (if (pred input)
              (do
                (setv halt? True)
                (reduced (ef acc)))))))

(defn xzip [iterable]
  (let [sentinel (object)
        it (iter iterable)]
    (make-transducer
      :step (let [x (try
                      (next it)
                      (except [StopIteration]
                        sentinel))]
              (if (is x sentinel)
                (reduced acc)
                (rf acc #(x input)))))))

(def cat
  (make-transducer
    :step (reduce-step rf acc input)))

(defn take [n]
  (assert (>= n 0))
  (comp
    (xzip (iterate inc 0))
    (make-transducer
      :step (let [[index input] input]
              (if (>= index n)
                (reduced acc)
                (rf acc input))))))
