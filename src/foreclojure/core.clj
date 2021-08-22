(ns foreclojure.core
  (require [clojure.string]))

(defn product-digits
  "Write a function which multiplies two numbers and
  returns the result as a sequence of its digits."
  [x y]
  (let [product (* x y)
        num-str (str product)
        tokens (into [] num-str)
        tokens (map str tokens)]
    (map #(Integer/parseInt %) tokens)))

; https://www.4clojure.com/problem/59
(defn juxtaposition
  "Take a set of functions and return a new function that takes a
  variable number of arguments and returns a sequence containing the
  result of applying each function left-to-right to the argument list."
  [& fns]
  (fn [& args]
    (map #(apply % args) fns)))

; https://www.4clojure.com/problem/65
(defn black-box-testing
  "Write a function which takes a collection and returns one of :map, :set,
  :list, or :vector - describing the type of collection it was given. You
  won't be allowed to inspect their class or use the built-in predicates like
  list? - the point is to poke at them and understand their behavior."
  [coll]
  (let [sample (empty coll)]
    (cond
      (= {} sample) :map
      (= #{} sample) :set
      (= (conj sample 1 2) [1 2]) :vector
      :else :list)))

; https://www.4clojure.com/problem/66
(defn greatest-common-divisor
  "Given two integers, write a function which returns the greatest common
  divisor."
  [a b]
  (cond
    (= a 0) b
    (= b 0) a
    (= a b) a
    (> a b) (greatest-common-divisor (- a b) b)
    :else (greatest-common-divisor a (- b a))))

; https://www.4clojure.com/problem/70
(defn word-sort
  "Write a function that splits a sentence up into a sorted list of words.
  Capitalization should not affect sort order and punctuation should be
  ignored."
  [s]
  (sort-by clojure.string/lower-case (re-seq #"\w+" s)))

; https://www.4clojure.com/problem/73
(defn three-in-row? [row]
  (cond
    (every? #(= % :x) row) :x
    (every? #(= % :o) row) :o
    :else nil))

#_(defn get-slice [board x y dx dy]
    [(get-in board [x y])
     (get-in board [(+ x dx) (+ y dy)])
     (get-in board [(+ x dx dx) (+ y dy dy)])])

#_(defn ttt [board]
    (let [dirs [[0 1] [1 0] [1 1] [1 -1]]]

      ))

(defn tic-tac-toe
  "Write a function which analyzes a tic-tac-toe board and returns :x if X has
  won, :o if O has won, and nil if neither player has won."
  [board]
  (let [rows board
        cols (apply map list board)
        diags [[(get-in board [0 0]) (get-in board [1 1]) (get-in board [2 2]) ]
               [(get-in board [0 2]) (get-in board [1 1]) (get-in board [2 0])]]
        slices (concat rows cols diags)]
    (some three-in-row? slices)))

; https://www.4clojure.com/problem/74
(defn filter-perfect-squares
  "Given a string of comma separated integers, write a function which returns
  a new comma separated string that only contains the numbers which are perfect
  squares."
  [s]
  ; split the string
  ; filter across it with a square pred
  (letfn [(perfect-square? [x]
            (let [root (int (Math/sqrt x))]
              (= (* root root) x)))]
    (->> (clojure.string/split s #",")
         (map #(Integer/parseInt %))
         (filter perfect-square?)
         (clojure.string/join ","))))

; https://4clojure.oxal.org/#/problem/77
(defn anagram-finder
  "Write a function which finds all the anagrams in a vector of words.
   Your function should return a set of sets.
   Words without any anagrams should be excluded from the result."
  [xs]
  (->> xs
       (group-by sort)
       vals
       (filter #(> (count %) 1))
       (map set)
       set))


; https://www.4clojure.com/problem/78
(defn trampo
  "Implement your own trampoline function."
  [f & args]
  ; TODO: Does this get tail-call optimized, or should I use loop/recur instead
  ; of the recursive call?
  (let [ret (apply f args)]
    (if (fn? ret)
      (trampo ret)
      ret)))



; https://www.4clojure.com/problem/85
(defn power-set [s]
  ; TODO: Speed this up. This is too slow and times out on 4Clojure.
  (if (empty? s)
    #{#{}}
    (into #{s} (mapcat power-set (for [x (apply sorted-set s)] (disj s x))))))

#_(defn power-set [s]
    (let [create-subset
          (fn [to from]
            (if (empty? from)
              #{#{}}
              (let [item (first from)]
                (create-subset (conj to (item)) (rest from))
                (create-subset to (rest from)))]))

; https://www.4clojure.com/problem/86
(defn happy-number? [n]
  (loop [curr-num n
         seen #{}]
    (let [digits (map #(Character/digit % 10) (str curr-num))
          square (int (apply + (map #(* % %) digits)))]
      (cond
        (contains? seen curr-num) false
        (= square 1) true
        :else (recur square (conj seen curr-num))))))

; https://www.4clojure.com/problem/90
(defn cartesian-product
  "Write a function which calculates the Cartesian product of two sets."
  [a b]
  (set (for [x a y b] [x y])))

; https://www.4clojure.com/problem/93
(defn leaf? [x]
  (not-any? sequential? x))

(defn partially-flatten [xs]
  (reduce
    (fn [flattened x]
      (if (leaf? x)
        (conj flattened x)
        (into flattened (partially-flatten x))))
    []
    xs))

; https://www.4clojure.com/problem/95
(defn binary-tree? [t]
  (if (not= 3 (count t))
    false
    (let [[root left right] t]
      (and
        (if (coll? left)
          (binary-tree? left)
          (nil? left))
        (if (coll? right)
          (binary-tree? right)
          (nil? right)))))))

; This solution is cleaner because we consider nil to be a valid binary tree,
; eliminating a lot of the checks along the way in the above soln.
(defn binary-tree? [t]
  (or
    (nil? t)
    (and
      (coll? t)
      (= (count t) 3)
      (every? binary-tree? (rest t)))))

; https://www.4clojure.com/problem/96
(defn symmetric? [t]
  (letfn [(mirror? [t1 t2]
            (or
              (and (nil? t1) (nil? t2))
              (and (= (first t1) (first t2))
                   (mirror? (nth t1 1) (nth t2 2))
                   (mirror? (nth t1 2) (nth t2 1)))))]
    (mirror? (nth t 1) (nth t 2))))

; https://www.4clojure.com/problem/97
(defn pascal-triangle
  "Pascal's triangle is a triangle of numbers computed using the following
  rules:
  - The first row is 1.
  - Each successive row is computed by adding together adjacent numbers in
    the  row above, and adding a 1 to the beginning and end of the row.
  Write a function which returns the nth row of Pascal's Triangle."
  [n]
  (loop [row [1]
         idx 1]
    (if (= idx n)
      row
      (recur
        (vec
          (concat
            [1]
            (->> (partition 2 1 row)
                 (mapv #(apply + %)))
            [1]))
        (inc idx)))))

; https://www.4clojure.com/problem/98
(defn equiv-classes
  "a is equivalent to b with respect to f if and only if (f a) is equal to
  (f b). Write a function with arguments f and D that computes the
  equivalence classes of D with respect to f"
  [f d]
  (->> (group-by f d)
       vals
       (map set)
       set))

; https://www.4clojure.com/problem/100
(defn least-common-multiple [ & xs]
  )

; https://www.4clojure.com/problem/102
(defn into-camel-case
  "When working with java, you often need to create an object with
  fieldsLikeThis, but you'd rather work with a hashmap that has :keys-like-this
  until it's time to convert. Write a function which takes lower-case
  hyphen-separated strings and converts them to camel-case strings."
  [s]
  (let [[x & xs] (clojure.string/split s #"-")
        capitalized-xs (map clojure.string/capitalize xs)
        tokens (apply vector x capitalized-xs)]
    (clojure.string/join tokens)))

; https://www.4clojure.com/problem/103
(defn k-combinations
  "Given a sequence S consisting of n elements generate all k-combinations of S,
  i. e. generate all possible sets consisting of k distinct elements taken
  from S."
  [k xs]
  (letfn [(k-comb-rec [combos combo xs]
            (if (= (count combo) k)
              #{combo}
              (when (not-empty xs)
                (set¡
                  (concat
                    combos
                    (k-comb-rec combos (conj combo (first xs)) (rest xs))
                    (k-comb-rec combos combo (rest xs)))))))]
    (k-comb-rec #{} #{} xs)))

; https://www.4clojure.com/problem/105
(defn identify-k-v [s]
  ; oops; this implementation fails because it overlooked an edge-case
  ; where we may have multiple keywords in a row, e.g. [:a :b] => {:a [] :b []},
  ; and by partitioning we lose this signal.
  (let [partitions (partition-by keyword? s)
        ks (flatten (filter #(keyword? (first %)) partitions))
        vs (remove #(keyword? (first %)) partitions)]
    (apply hash-map (interleave ks vs))))

; https://www.4clojure.com/problem/114
(defn global-take-while
  "Write a function which accepts an integer n, a predicate p, and a sequence.
  It should return a lazy sequence of items in the list up to, but not
  including, the nth item that satisfies the predicate."
  [n p? xs]
  (if (p? (first xs))
    (let [n (dec n)]
      (if (zero? n)
        '()
        (lazy-seq (cons (first xs) (global-take-while n p? (rest xs))))))
    (lazy-seq (cons (first xs) (global-take-while n p? (rest xs))))))

; This solution has the problem that it includes the nth item
#_(defn global-take-while [n p? xs]
    (if (zero? n)
      '()
      ((lazy-seq
         (cons
           (first xs)
           (global-take-while
             (if (p? (first xs)) (dec n) n)
             p?
             (rest xs)))))))

; https://www.4clojure.com/problem/118
(defn m
  "Map is one of the core elements of a functional programming language.
  Given a function f and an input sequence s, return a lazy sequence of (f x)
  for each element x in s."
  [f [h & t :as xs]]
  (if (empty? xs)
    '()
    (lazy-seq (cons (f h) (m f t)))))

(fn [piece board]
  (letfn [(tic-tac-toe [board]
            (let [rows board
                  cols (apply map list board)
                  diags [[(get-in board [0 0]) (get-in board [1 1]) (get-in board [2 2]) ]
                         [(get-in board [0 2]) (get-in board [1 1]) (get-in board [2 0])]]
                  slices (concat rows cols diags)
                  three-in-row?
                  (fn [row]
                    (cond
                      (every? #(= % :x) row) :x
                      (every? #(= % :o) row) :o
                      :else nil))]
              (some three-in-row? slices)))]
    (let [next-row (fn [row col]
                     (if (= col 2)
                       (inc row)
                       row))
          next-col (fn [col]
                     (if (= col 2)
                       0
                       (inc col)))]
    (loop [wins #{}
           row 0
           col 0]
      (cond
        (= row 3)
        wins

        (and (= (get-in board [row col]) :e)
             (tic-tac-toe (assoc-in board [row col] piece)))
        (recur
          (conj wins [row col])
          (next-row row col)
          (next-col col))

        :else
        (recur
          wins
          (next-row row col)
          (next-col col)))))))

; https://www.4clojure.com/problem/119
(defn win-tic-tac-toe
  "Create a function that accepts a game piece and board as arguments, and
  returns a set (possibly empty) of all valid board placements of the game
  piece  which would result in an immediate win.\n\n "
  [piece board]
  ; find each :e
  ; replace with :piece
  ; if tic-tac-toe win, add to set
  (let [next-row (fn [row col]
                   (if (= col 2)
                     (inc row)
                     row))
        next-col (fn [col]
                   (if (= col 2)
                     0
                     (inc col)))]
    (loop [wins #{}
           row 0
           col 0]
      (cond
        (= row 3)
        wins

        (and (= (get-in board [row col]) :e)
             (tic-tac-toe (assoc-in board [row col] piece)))
        (recur
          (conj wins [row col])
          (next-row row col)
          (next-col col))

        :else
        (recur
          wins
          (next-row row col)
          (next-col col))))))

; https://www.4clojure.com/problem/120
(defn sum-square-digits
  "Write a function which takes a collection of integers as an argument.
  Return the count of how many elements are smaller than the sum of their
  squared component digits."
  [coll]
  (count
    (filter
      (fn [num]
        (let [digits (map #(Character/digit % 10) (str num))
              square (apply + (map #(* % %) digits))]
          (< num square)))
      coll)))

; https://www.4clojure.com/problem/121
(defn universal-computation-engine
  "Given a formula in prefix notation, return a function that accepts a
  mapping of variables to values and computes the result. Formulas may
  contain +,-,*,/,constants, and variables."
  [formula]
  (fn [symbol-table]
    (let [ops {'+ +, '- -, '* *, '/ / }
          evaluate
          (fn evaluate [exp]
            (cond
              (number? exp) exp
              (symbol? exp) (get symbol-table exp)
              :else
              (let [[op & operands] exp]
                (apply (get ops op) (map evaluate operands)))))]
      (evaluate formula))))

; https://www.4clojure.com/problem/122
(defn binary-to-decimal [s]
  (->> (reverse s)
       (map-indexed
         (fn [idx item]
           [idx (if (= item \1) 1 0)]))
       (reduce
         (fn [sum [power bin]]
           (+ sum (* bin (int (Math/pow 2 power)))))
         0)))



; https://www.4clojure.com/problem/126
(defn looking-glass
  "Choose an x that satisfies the following."
  (let [x java.lang.Class]
    (and (= (class x) x) x)))

; https://www.4clojure.com/problem/128
(defn recognize-playing-cards [[suit rank]]
  (let [suits {\S :spade
               \H :heart
               \D :diamond
               \C :club}
        ranks {\2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6 \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12}]
    {:suit (suits suit)
     :rank (ranks rank)}))

; https://www.4clojure.com/problem/132
; This solution works, but ended up being a little clunky.
; Initially, I thought chunking the list into tuples (using `partition-by`) would
; make it easy to assess the information I needed: each pair of adjacent elements.
; But dealing with the edge cases: that your final pair may only have one item, and that
; we need this function to be lazy, makes this code too clunky.
#_(defn insert-between [p? val coll]
    (let [partitions (partition-all 2 1 coll)
          insertions (map #(if
                             (and (= (count %) 2) (p? (first %) (last %)))
                             [(first %) val (last %)]
                             %) partitions)]
      (->> insertions
           (map (fn [t] (if (= 1 (count t)) (last t) (butlast t))))
           flatten)))

; So instead I moved to just build the lazy sequence from scratch. Less code, less edge cases.
(defn insert-between [p? val [a b :as coll]]
  (lazy-seq
    (when b
      (if (p? a b)
        (cons a (cons val (insert-between p? val (rest coll))))
        (cons a (insert-between p? val (rest coll)))))))

; This implementaiton was cleaner, but wasn't lazy, and one of the test cases
; involved an infinite sequence.
#_(defn insert-between [p? val coll]
    (reduce
      (fn [insertions item]
        (cond
          (empty? insertions) (conj insertions item)
          (p? (last insertions) item) (conj insertions val item)
          :else (conj insertions item)))
      []
      coll))

; https://www.4clojure.com/problem/135
(defn infix
  "Write a function that accepts a variable length mathematical expression
  consisting of numbers and the operations +, -, *, and /.
  Assume a simple calculator that does not do precedence
  and instead just calculates left to right."
  [& tokens]
  (loop [[left op right & tokens] tokens]
    (let [val (op left right)]
      (if (empty? tokens)
        val
        (recur (cons val tokens))))))

; https://4clojure.oxal.org/#/problem/141
(defn find-winner [trump-suit]
  (fn [cards]
    (let [lead-suit
          (:suit (first cards))

          trump-cards
          (->> cards
               (filter #(= (:suit %) trump-suit))
               (sort-by :rank))

          lead-cards
          (->> cards
               (filter #(= (:suit %) lead-suit))
               (sort-by :rank))]

      (or (last trump-cards)
          (last lead-cards)))))

(defn find-winner [trump-suit]
  (fn [cards]
    (let [trump-cards (sort-by :rank (filter #(= (:suit %) trump-suit) cards))
          trick-cards (sort-by :rank (filter #(= (:suit %) (:suit (first
                                                                    cards)))
                                             cards))]
      (or (last trump-cards)
          (last trick-cards)))))

#_(defn find-winner [trump-suit]
  (fn [cards]
    (let [lead-suit (:suit (first cards))
          trump-card? (fn [card] (= (:suit card) trump))]
      (loop [winning-trump-card nil
             winning-lead-card nil
             cards cards]
        (let [curr-card (first cards)]
          (cond
            (trump-card? curr-card)
            (if (or
                  (nil? winning-trump-card)
                  (> (:rank curr-card) (:rank winning-trump-card)))
              (recur curr-card winning-lead-card (rest cards))
              )
          )

        )
      (reduce (fn [winning-card curr-card]
                (if (trump-card? curr-card)
                  (if (trump-card? winning-card)
                    (> (:rank curr-card) (:rank))
                ))
      )
    )
  )

; https://www.4clojure.com/problem/143
(defn dot-product [a b]
  (apply + (map * a b)))

; https://www.4clojure.com/problem/144
(defn oscilrate [val & fs]
  (lazy-seq
    (cons val
          (apply oscilrate (cons ((first fs) val) (concat (rest fs) [(first fs)]))))))

; Better Implementation: Use `reductions` and `cycle` to eliminate the above impl's
; (`lazy-seq` and `concart rest/first` calls.
(defn oscilrate [val & fs]
  (reductions
    (fn [v f]
      (f v))
    val
    (cycle fs)))

; https://www.4clojure.com/problem/147
(defn pascal-trapezoid [s]
  (lazy-seq (cons s
                  (pascal-trapezoid
                    (concat [(first s)]
                            (->> (partition 2 1 s)
                                 (mapv #(apply +' %)))
                            [(last s)])))))

; Better solution:
; Iterate means we don't have to build the lazy-seq from scratch.
; (cons 0 %) (conj % 0) aligns the vectors so that we don't have
; to partition, concat, etc.
(defn p-t [s]
  (iterate
    #(mapv + (cons 0 %) (conj % 0))
    s))

; https://www.4clojure.com/problem/150
(defn palindromic-numbers [n]
  )

; https://www.4clojure.com/problem/153
(defn disjoint?
  "Given a set of sets, create a function which returns true if no two
  of those sets have any elements in common1 and false otherwise. "
  [s]
  (let [num-elems (->> (map count s) (reduce +))
        super-set (apply concat s)]
    (= num-elems (count super-set))))

; Better? shorter solution; requires clojure.set which 4clojure disallows. (I think).
; #(= (->> (map count %) (reduce +)) (count (apply clojure.set/union %)))

; https://www.4clojure.com/problem/157
(defn index-seq
  "Transform a sequence into a sequence of pairs containing the original
  elements along with their index."
  [xs]
  (into [] (zipmap xs (range (count xs)))))

; https://4clojure.oxal.org/#/problem/158
(defn decurry
  "Write a function that accepts a curried function of unknown arity n.
  Return an equivalent function of n arguments."
  [curried-fn]
  (fn [& args]
    (reduce #(% %2) curried-fn args)))

; https://www.4clojure.com/problem/171
(defn intervals
  "Write a function that takes a sequence of integers and returns a sequence of
  \"intervals\". Each interval is a a vector of two integers, start and end,
  such that all integers between start and end (inclusive) are contained in
  the input sequence."
  [x]
  (let [sorted (sort (distinct x))
        groups (reduce
                 (fn [groups x]
                   (let [curr (last (last groups))]
                     (cond
                       ; Edge case to handle our first iteration; we haven't created
                       ; any grouping yet so curr will be nil.
                       (= curr nil) (conj groups [x])
                       ; If this is a contiguous integer, add it to the most recent group.
                       (= (- x curr) 1) (assoc-in groups [(dec (count groups)) (count (last groups))] x)
                       ; Otherwise, start a new group.
                       :else (conj groups [x]))))
                 []
                 sorted)]
    ; Convert our groups, e.g. [1 2 3] into intervals, e.g. [1 3]
    (reduce
      (fn [intervals group]
        (conj intervals [(first group) (last group)])),
      []
      groups)))

; https://www.4clojure.com/problem/177
(defn balanced? [s]
  ; This implementaiton has a bug in that it only checks that num open matches num closed. Doesn't check ordering.
  ; So it will accept "[{]}", which is incorrect.
  (let [pairings {\[ \], \{ \}, \( \)}]
    (loop [unbalanced-map {\] 0 \} 0 \) 0}
           s (seq s)]
      (let [curr-char (first s)]
        (cond
          (nil? curr-char)
          true

          (#{ \]\)\} } curr-char)
          (let [unbalanced-map (update unbalanced-map curr-char dec)]
            (if (some neg? (vals unbalanced-map))
              false
              (recur unbalanced-map (rest s))))

          (#{\[ \( \{} curr-char)
          (let [unbalanced-map (update unbalanced-map (pairings curr-char) inc)]
            (recur unbalanced-map (rest s)))

          :else
          (recur unbalanced-map (rest s)))))))

(defn balanced? [s]
  (empty?
    (reduce
      (fn [stack curr-char]
        (cond
          (#{\( \{ \[} curr-char)
          (conj stack curr-char)

          (#{\) \} \]} curr-char)
          (let [prev (peek stack)
                match (get {\) \(, \} \{, \] \[} curr-char)]
            (if (= prev match)
              (pop stack)
              ; If they didn't match, this string isn't balanced because tokens aren't properly paired.
              ; Just push the closed token on, which will never get popped, as a sentinel.
              ; When reduce finishes, we won't have an empty stack.
              (conj stack curr-char)))

          :else
          stack))
      []
      s)))

; https://www.4clojure.com/problem/195
(defn gen-parens-A
  "Generate all possible combinations of well-formed parentheses of length 2n
  (n pairs of parentheses). Return as a set of strings, e.g. ,#{'()()', '(())'}."
  [n]
  (letfn [(gen-parens-rec [paren-str num-to-open num-unclosed]
            (if (every? zero? [num-unclosed num-to-open])
              #{paren-str}
              (set (concat
                     (if (pos? num-to-open)
                       (gen-parens-rec (str paren-str "(") (dec num-to-open) (inc num-unclosed))
                       #{})
                     (if (pos? num-unclosed)
                       (gen-parens-rec (str paren-str ")") num-to-open (dec num-unclosed))
                       #{})))))]
    (gen-parens-rec "" n 0)))

(defn identify-k-v [s]
  (loop [m {}
         k (first s)
         v []
         s (rest s)]
    (if (empty? s)
      (if (nil? k)
        m
        (assoc m k v))
      (let [curr (first s)]
        (if (keyword? curr)
          (recur (assoc m k v) curr [] (rest s))
          (recur m k (conj v curr) (rest s)))))))

;;;
; Game of Life
; Clojure Programming Textbook Example
; TODO: Move to its own project
;;;
(defn empty-board
  "Creates a rectangular empty board of the specified width
   ahd height."
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  "Turns :on each of the cells specified as [y, x] coordiantes."
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))

(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))

(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn count-neighbours
  [board loc]
  (count (filter #(get-in board %) (neighbours loc))))

(defn indexed-step
  "Yields the next state of the board, using indices to determine neighbors,
   liveness, etc."
  [board]
  (let [w (count board)
        h (count (first board))]
    (loop [new-board board
           x 0
           y 0]
      (cond
        (>= x w) new-board
        (>= y h) (recur new-board (inc x) 0)
        :else
        (let [new-liveness
              (case (count-neighbours board [x y])
                2 (get-in board [x y])
                3 :on
                nil)]
          (recur (assoc-in new-board [x y] new-liveness) x (inc y)))))))

(defn window
  "Returns a lazy sequence of 3-item windows centered around each item of coll."
  [coll]
  (partition 3 1 (concat [nil] coll [nil])))

(defn cell-block
  [[left mid right]]
  (window (map vector (or left (repeat nil)) mid (or right (repeat nil)))))

(defn cell-block
  "Creates a sequence of 3x3 windows from a triple of 3 sequences."
  [[left mid right]]
  (window (map vector left mid right)))

(defn step
  "Yields the next state of the world."
  [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

