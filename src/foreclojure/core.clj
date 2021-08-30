(ns foreclojure.core
  (:require [clojure.string]))

; https://4clojure.oxal.org/#/problem/19
(defn last*
  "Write a function which returns the last element in the sequence.
   Restrictions: `last`."
  [xs]
  (nth xs (dec (count xs))))

(comment
  (def __ last*)
  (= (__ [1 2 3 4 5]) 5)
  (= (__ '(5 4 3)) 3)
  (= (__ ["b" "c" "d"]) "d"))

; https://4clojure.oxal.org/#/problem/20
(defn penultimate
  "Write a function which returns the second to last element from a sequenence."
  [xs]
  (last (butlast xs)))

(comment
  (def __ penultimate)
  (= (__ (list 1 2 3 4 5)) 4)
  (= (__ ["a" "b" "c"]) "b")
  (= (__ [[1 2] [3 4]]) [1 2]))

; https://4clojure.oxal.org/#/problem/21
(defn nth*
  "Write a fn which returns the Nth elem from a sequence.
  Restrictions: nth."
  [xs n]
  (first (drop n xs)))

(comment
  (def __ nth*)
  (= (__ '(4 5 6 7) 2) 6)
  (= (__ [:a :b :c] 0) :a)
  (= (__ [1 2 3 4] 1) 2)
  (= (__ '([1 2] [3 4] [5 6]) 2) [5 6]))

; https://4clojure.oxal.org/#/problem/22
(defn count*
  "Write a function which returns the total number of elements in the sequence.
  Restriction: count."
  [xs]
  ;(reduce (fn [num-elems _] (inc num-elems)) 0 xs))
  (loop [num-elems 0
         xs xs]
    (if (empty? xs)
      num-elems
      (recur (inc num-elems) (rest xs)))))

(comment
  (def __ count*)
  (= (__ '(1 2 3 3 1)) 5)
  (= (__ "Hello World") 11)
  (= (__ [[1 2] [3 4] [5 6]]) 3)
  (= (__ '(13)) 1)
  (= (__ '(:a :b :c)) 3))

; https://4clojure.oxal.org/#/problem/46
(defn flip
  "Write a higher-order function which flips the order of the arguments of an
  input function."
  [f]
  (fn [& args]
    (let [flipped (reverse args)]
      (apply f flipped))))

(comment
  (def __ flip)
  (= 3 ((__ nth) 2 [1 2 3 4 5]))
  (= true ((__ >) 7 8))
  (= 4 ((__ quot) 2 8))
  (= [1 2 3] ((__ take) [1 2 3 4 5] 3)))

; https://4clojure.oxal.org/#/problem/59
(defn juxtaposition
  "Take a set of functions and return a new function that takes a
  variable number of arguments and returns a sequence containing the
  result of applying each function left-to-right to the argument list."
  [& fns]
  (fn [& args]
    (map #(apply % args) fns)))

(comment
  (def __ juxtaposition)
  (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
  (= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello"))
  (= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))

; https://4clojure.oxal.org/#/problem/65
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

(comment
  (def __ black-box-testing)
  (= :map (__ {:a 1, :b 2}))
  (= :list (__ (range (rand-int 20))))
  (= :vector (__ [1 2 3 4 5 6]))
  (= :set (__ #{10 (rand-int 5)}))
  (= [:map :set :vector :list] (map __ [{} #{} [] ()])))

; https://4clojure.oxal.org/#/problem/66
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

(comment
  (def __ greatest-common-divisor)
  (= (__ 2 4) 2)
  (= (__ 10 5) 5)
  (= (__ 5 7) 1)
  (= (__ 1023 858) 33))

; https://4clojure.oxal.org/#/problem/70
(defn word-sort
  "Write a function that splits a sentence up into a sorted list of words.
  Capitalization should not affect sort order and punctuation should be
  ignored."
  [s]
  (sort-by clojure.string/lower-case (re-seq #"\w+" s)))

(comment
  (def __ word-sort)

  (= (__  "Have a nice day.")
     ["a" "day" "Have" "nice"])

  (= (__  "Clojure is a fun language!")
     ["a" "Clojure" "fun" "is" "language"])

  (= (__  "Fools fall for foolish follies.")
     ["fall" "follies" "foolish" "Fools" "for"]))

; https://4clojure.oxal.org/#/problem/73
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

(comment
  (def __ tic-tac-toe)

  (= nil (__ [[:e :e :e]
              [:e :e :e]
              [:e :e :e]]))

  (= :x (__ [[:x :e :o]
             [:x :e :e]
             [:x :e :o]]))

  (= :o (__ [[:e :x :e]
             [:o :o :o]
             [:x :e :x]]))

  (= nil (__ [[:x :e :o]
              [:x :x :e]
              [:o :x :o]]))

  (= :x (__ [[:x :e :e]
             [:o :x :e]
             [:o :e :x]]))

  (= :o (__ [[:x :e :o]
             [:x :o :e]
             [:o :e :x]]))

  (= nil (__ [[:x :o :x]
              [:x :o :x]
              [:o :x :o]])))

; https://4clojure.oxal.org/#/problem/74
(defn filter-perfect-squares
  "Given a string of comma separated integers, write a function which returns
  a new comma separated string that only contains the numbers which are perfect
  squares."
  [s]
  (letfn [(perfect-square? [x]
            (let [root (int (Math/sqrt x))]
              (= (* root root) x)))]
    (->> (clojure.string/split s #",")
         (map #(Integer/parseInt %))
         (filter perfect-square?)
         (clojure.string/join ","))))

(comment
  (def __ filter-perfect-squares)
  (= (__ "4,5,6,7,8,9") "4,9")
  (= (__ "15,16,25,36,37") "16,25,36"))

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

(comment
  (def __ anagram-finder)

  (= (__ ["meat" "mat" "team" "mate" "eat"])
     #{#{"meat" "team" "mate"}})

  (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
     #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}))

; https://4clojure.oxal.org/#/problem/78
(defn trampo
  "Implement your own trampoline function."
  [f & args]
  ; TODO: Does this get tail-call optimized, or should I use loop/recur instead
  ; of the recursive call?
  (let [ret (apply f args)]
    (if (fn? ret)
      (trampo ret)
      ret)))

(comment
  (def __ trampo)

  (= (letfn [(triple [x] #(sub-two (* 3 x)))
             (sub-two [x] #(stop?(- x 2)))
             (stop? [x] (if (> x 50) x #(triple x)))]
       (__ triple 2))
     82)

  (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
             (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
       (map (partial __ my-even?) (range 6)))
     [true false true false true false]))

; https://4clojure.oxal.org/#/problem/85
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
                (create-subset to (rest from)))))]))

(comment
  (def __ power-set)

  (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})

  (= (__ #{}) #{#{}})

  (= (__ #{1 2 3})
     #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})

  (= (count (__ (into #{} (range 10)))) 1024))

; https://4clojure.oxal.org/#/problem/86
(defn happy-number? [n]
  (loop [curr-num n
         seen #{}]
    (let [digits (map #(Character/digit % 10) (str curr-num))
          square (int (apply + (map #(* % %) digits)))]
      (cond
        (contains? seen curr-num) false
        (= square 1) true
        :else (recur square (conj seen curr-num))))))

(comment
  (def __ happy-number?)
  (= (__ 7) true)
  (= (__ 986543210) true)
  (= (__ 2) false)
  (= (__ 3) false))

; https://4clojure.oxal.org/#/problem/90
(defn cartesian-product
  "Write a function which calculates the Cartesian product of two sets."
  [a b]
  (set (for [x a y b] [x y])))

(comment
  (def __ cartesian-product)

  (= (__ #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
     #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
       ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
       ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})

  (= (__ #{1 2 3} #{4 5})
     #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})

  (= 300 (count (__ (into #{} (range 10))
                    (into #{} (range 30))))))

; https://4clojure.oxal.org/#/problem/93
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

(comment
  (def __ partially-flatten)

  (= (__ [["Do"] ["Nothing"]])
     [["Do"] ["Nothing"]])

  (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
     [[:a :b] [:c :d] [:e :f]])

  (= (__ '((1 2)((3 4)((((5 6)))))))
     '((1 2)(3 4)(5 6))))

; https://4clojure.oxal.org/#/problem/95
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
          (nil? right))))))

; This solution is cleaner because we consider nil to be a valid binary tree,
; eliminating a lot of the checks along the way in the first cut I took above.
(defn binary-tree?
  "Write a predicate which checks whether or not a given sequence represents a
  binary tree. Each node in the tree must have a value, a left child, and a
  right child."
  [t]
  (or
    (nil? t)
    (and
      (coll? t)
      (= (count t) 3)
      (every? binary-tree? (rest t)))))

(comment
  (def __ binary-tree?)
  (= (__ '(:a (:b nil nil) nil))
     true)

  (= (__ '(:a (:b nil nil)))
     false)

  (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]])
     true)

  (= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]])
     false))

; https://4clojure.oxal.org/#/problem/96
(defn symmetric?
  "Let us define a binary tree as \"symmetric\" if the left half of the tree
  is the mirror image of the right half of the tree. Write a predicate to
  determine whether or not a given binary tree is symmetric."
  [t]
  (letfn [(mirror? [t1 t2]
            (or
              (and (nil? t1) (nil? t2))
              (and (= (first t1) (first t2))
                   (mirror? (nth t1 1) (nth t2 2))
                   (mirror? (nth t1 2) (nth t2 1)))))]
    (mirror? (nth t 1) (nth t 2))))


(comment
  (def __ symmetric?)

  (= (__ '(:a (:b nil nil) (:b nil nil))) true)
  (= (__ '(:a (:b nil nil) nil)) false)
  (= (__ '(:a (:b nil nil) (:c nil nil))) false)

  (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
     true)

  (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
     false)

  (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] nil]] nil]])
     false))

; https://4clojure.oxal.org/#/problem/97
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

(comment
  (def __ pascal-triangle)
  (= (__ 1) [1])

  (= (map __ (range 1 6))
     [     [1]
      [1 1]
      [1 2 1]
      [1 3 3 1]
      [1 4 6 4 1]])

  (= (__ 11)
     [1 10 45 120 210 252 210 120 45 10 1])

  (= (__ 11)
     [1 10 45 120 210 252 210 120 45 10 1]))

; https://4clojure.oxal.org/#/problem/98
(defn equiv-classes
  "a is equivalent to b with respect to f if and only if (f a) is equal to
  (f b). Write a function with arguments f and D that computes the
  equivalence classes of D with respect to f"
  [f d]
  (->> (group-by f d)
       vals
       (map set)
       set))

(comment
  (def __ equiv-classes)

  (= (__ #(* % %) #{-2 -1 0 1 2})
     #{#{0} #{1 -1} #{2 -2}})

  (= (__ #(rem % 3) #{0 1 2 3 4 5 })
     #{#{0 3} #{1 4} #{2 5}})

  (= (__ identity #{0 1 2 3 4})
     #{#{0} #{1} #{2} #{3} #{4}})

  (= (__ (constantly true) #{0 1 2 3 4})
     #{#{0 1 2 3 4}}))

; https://4clojure.oxal.org/#/problem/99
(defn product-digits
  "Write a function which multiplies two numbers and
  returns the result as a sequence of its digits."
  [x y]
  (let [product (* x y)
        num-str (str product)
        tokens (into [] num-str)
        tokens (map str tokens)]
    (map #(Integer/parseInt %) tokens)))

(comment
  (def __ product-digits)
  (= (__ 1 1) [1])
  (= (__ 99 9) [8 9 1])
  (= (__ 999 99) [9 8 9 0 1]))

; https://4clojure.oxal.org/#/problem/100
(defn least-common-multiple [ & xs]
  )

(comment
  (def __ least-common-multiple)
  (= (__ 2 3) 6)
  (= (__ 5 3 7) 105)
  (= (__ 1/3 2/5) 2)
  (= (__ 3/4 1/6) 3/2))

; https://4clojure.oxal.org/#/problem/102
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

(comment
  (def __ into-camel-case)
  (= (__ "something") "something")
  (= (__ "multi-word-key") "multiWordKey")
  (= (__ "leaveMeAlone") "leaveMeAlone"))

; https://4clojure.oxal.org/#/problem/103
(defn k-combinations
  "Given a sequence S consisting of n elements generate all k-combinations of S,
  i. e. generate all possible sets consisting of k distinct elements taken
  from S."
  [k xs]
  (letfn [(k-comb-rec [combos combo xs]
            (if (= (count combo) k)
              #{combo}
              (when (not-empty xs)
                (set
                  (concat
                    combos
                    (k-comb-rec combos (conj combo (first xs)) (rest xs))
                    (k-comb-rec combos combo (rest xs)))))))]
    (k-comb-rec #{} #{} xs)))

(comment
  (def __ k-combinations)
  (= (__ 1 #{4 5 6}) #{#{4} #{5} #{6}})
  (= (__ 10 #{4 5 6}) #{})
  (= (__ 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})

  (= (__ 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                           #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})

  (= (__ 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})

  (= (__ 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                        #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))

; https://4clojure.oxal.org/#/problem/105
(defn identify-k-v
  "Given an input sequence of keywords and numbers, create a map such that each
  key in the map is a keyword, and the value is a sequence of all the numbers
  (if any) between it and the next keyword in the sequence."
  [s]
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

(comment
  (def __ identify-k-v)
  (= {} (__ []))
  (= {:a [1]} (__ [:a 1]))

  (= {:a [1]
      :b [2]} (__ [:a 1, :b 2]))

  (= {:a [1 2 3]
      :b []
      :c [4]} (__ [:a 1 2 3 :b :c 4])))

#_(defn identify-k-v [s]
    ; oops; this first attempt fails because it overlooks an edge-case
    ; where we may have multiple keywords in a row, e.g. [:a :b] => {:a [] :b []},
    ; and by partitioning we lose this signal. It was alluring though -- less
    ; code!
    (let [partitions (partition-by keyword? s)
          ks (flatten (filter #(keyword? (first %)) partitions))
          vs (remove #(keyword? (first %)) partitions)]
      (apply hash-map (interleave ks vs))))

; https://4clojure.oxal.org/#/problem/114
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

; This first attempt incorrectly includes the nth item.
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

(comment
  (def __ global-take-while)

  (= [2 3 5 7 11 13]
     (__ 4 #(= 2 (mod % 3))
         [2 3 5 7 11 13 17 19 23]))

  (= ["this" "is" "a" "sentence"]
     (__ 3 #(some #{\i} %)
         ["this" "is" "a" "sentence" "i" "wrote"]))

  (= ["this" "is"]
     (__ 1 #{"a"}
         ["this" "is" "a" "sentence" "i" "wrote"])))

; https://4clojure.oxal.org/#/problem/118
(defn m
  "Map is one of the core elements of a functional programming language.
  Given a function f and an input sequence s, return a lazy sequence of (f x)
  for each element x in s."
  [f [h & t :as xs]]
  (if (empty? xs)
    '()
    (lazy-seq (cons (f h) (m f t)))))

(comment
  (def __ m)

  (= [3 4 5 6 7]
     (__ inc [2 3 4 5 6]))

  (= (repeat 10 nil)
     (__ (fn [_] nil) (range 10)))

  (= [1000000 1000001]
     (->> (__ inc (range))
          (drop (dec 1000000))
          (take 2)))

  (= [1000000 1000001]
     (->> (__ inc (range))
          (drop (dec 1000000))
          (take 2))))

; https://4clojure.oxal.org/#/problem/119
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

(comment
  (def __ win-tic-tac-toe)

  (= (__ :x [[:o :e :e]
             [:o :x :o]
             [:x :x :e]])
     #{[2 2] [0 1] [0 2]})

  (= (__ :x [[:x :o :o]
             [:x :x :e]
             [:e :o :e]])
     #{[2 2] [1 2] [2 0]})

  (= (__ :x [[:x :e :x]
             [:o :x :o]
             [:e :o :e]])
     #{[2 2] [0 1] [2 0]})

  (= (__ :x [[:x :x :o]
             [:e :e :e]
             [:e :e :e]])
     #{})

  (= (__ :o [[:x :x :o]
             [:o :e :o]
             [:x :e :e]])
     #{[2 2] [1 1]}))

; This scopes the above `win-tic-tac-toe` and helper `tic-tac-toe` under a
; single name to submit on 4clojure.
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

; https://4clojure.oxal.org/#/problem/120
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

(comment
  (def __ sum-square-digits)
  (= 8 (__ (range 10)))
  (= 19 (__ (range 30)))
  (= 50 (__ (range 100)))
  (= 50 (__ (range 1000))))

; https://4clojure.oxal.org/#/problem/121
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

(comment
  (def __ universal-computation-engine)

  (= 2 ((__ '(/ a b))
        '{b 8
          a 16}))

  (= 8 ((__ '(+ a b 2))
        '{a 2
          b 4}))

  (= [6 0 -4]
     (map (__ '(* (+ 2 a)
                  (- 10 b)))
          '[{a 1
             b 8}
            {b 5
             a -2}
            {a 2
             b 11}]))

  (= 1 ((__ '(/ (+ x 2)
                (* 3 (+ y 1))))
        '{x 4
          y 1})))

; https://4clojure.oxal.org/#/problem/122
(defn binary-to-decimal
  "Convert a binary number, provided in the form of a string, to its numerical
  value."
  [s]
  (->> (reverse s)
       (map-indexed
         (fn [idx item]
           [idx (if (= item \1) 1 0)]))
       (reduce
         (fn [sum [power bin]]
           (+ sum (* bin (int (Math/pow 2 power)))))
         0)))

(comment
  (def __ binary-to-decimal)
  (= 0     (__ "0"))
  (= 7     (__ "111"))
  (= 8     (__ "1000"))
  (= 9     (__ "1001"))
  (= 255   (__ "11111111"))
  (= 1365  (__ "10101010101"))
  (= 65535 (__ "1111111111111111")))

; https://4clojure.oxal.org/#/problem/126
(defn looking-glass
  "Choose an x that satisfies the following."
  []
  (let [x java.lang.Class]
    (and (= (class x) x) x)))

(comment
  (looking-glass))

; https://4clojure.oxal.org/#/problem/128
(defn recognize-playing-cards
  "For purposes of determining rank, we will define the cards to be valued
  from 0 (the two) to 12 (the ace). Write a function which converts
  (for example) the string \"SJ\" into a map of {:suit :spade,:rank 9}.
  A ten will always be represented with the single character \"T\", rather than
  the two characters \"10\"."
  [[suit rank]]
  (let [suits {\S :spade
               \H :heart
               \D :diamond
               \C :club}
        ranks {\2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6 \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12}]
    {:suit (suits suit)
     :rank (ranks rank)}))

(comment
  (def __ recognize-playing-cards)

  (= {:suit :diamond :rank 10} (__ "DQ"))

  (= {:suit :heart :rank 3} (__ "H5"))

  (= {:suit :club :rank 12} (__ "CA"))

  (= (range 13) (map (comp :rank __ str)
                     '[S2 S3 S4 S5 S6 S7
                       S8 S9 ST SJ SQ SK SA])))

; https://4clojure.oxal.org/#/problem/132
; This solution works, but ended up being a little clunky.
; Initially, I thought chunking the list into tuples (using `partition-by`) would
; make it easy to assess the information I needed: each pair of adjacent elements.
; But dealing with the edge cases: that your final pair may only have one item,
; and that we need this function to be lazy, makes this code too clunky.
(defn insert-between [p? val coll]
    (let [partitions (partition-all 2 1 coll)
          insertions (map #(if
                             (and (= (count %) 2) (p? (first %) (last %)))
                             [(first %) val (last %)]
                             %) partitions)]
      (->> insertions
           (map (fn [t] (if (= 1 (count t)) (last t) (butlast t))))
           flatten)))

; So instead I moved to just build the lazy sequence from scratch.
; Less code, less edge cases. TODO: Look into bug dropping final elem.
#_(defn insert-between
  "Write a function that takes a two-argument predicate, a value, and a
  collection; and returns a new collection where the value is inserted between
  every two items that satisfy the predicate."
  [p? val [a b :as coll]]
  (lazy-seq
    (when b
      (if (p? a b)
        (cons a (cons val (insert-between p? val (rest coll))))
        (cons a (insert-between p? val (rest coll)))))))

; This implementation was cleaner, but wasn't lazy, and one of
; the test cases
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

(comment
  (def __ insert-between)
  (= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))
  (= '(2) (__ > :more [2]))
  (= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))
  (empty? (__ > :more ()))

  (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
     (take 12 (->> [0 1]
                   (iterate (fn [[a b]] [b (+ a b)]))
                   (map first) ; fibonacci numbers
                   (__ (fn [a b] ; both even or both odd
                         (= (mod a 2) (mod b 2)))
                       :same)))))

; https://4clojure.oxal.org/#/problem/135
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

(comment
  (def __ infix)
  (= 7  (__ 2 + 5))
  (= 42 (__ 38 + 48 - 2 / 2))
  (= 8  (__ 10 / 2 - 1 * 2))
  (= 72 (__ 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)))

; https://4clojure.oxal.org/#/problem/141
(defn find-winner
  " devise a function that can determine which of a number of cards has won a
  trick. You should accept a trump suit, and return a function winner.
  Winner will be called on a sequence of cards, and should return the one which
  wins the trick."
  [trump-suit]
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

(comment
  (def __ find-winner)

  (let [notrump (__ nil)]
    (and (= {:suit :club :rank 9}  (notrump [{:suit :club :rank 4}
                                             {:suit :club :rank 9}]))
         (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                             {:suit :club :rank 10}]))))

  (= {:suit :club :rank 10} ((__ :club) [{:suit :spade :rank 2}
                                         {:suit :club :rank 10}]))
  (= {:suit :heart :rank 8}
     ((__ :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                   {:suit :diamond :rank 10} {:suit :heart :rank 4}])))

; https://4clojure.oxal.org/#/problem/143
(defn dot-product
  "Create a function that computes the dot product of two sequences. You may
  assume that the vectors will have the same length."
  [a b]
  (apply + (map * a b)))

(comment
  (def __ dot-product)
  (= 0 (__ [0 1 0] [1 0 0]))
  (= 3 (__ [1 1 1] [1 1 1]))
  (= 32 (__ [1 2 3] [4 5 6]))
  (= 256 (__ [2 5 6] [100 10 1])))


; https://4clojure.oxal.org/#/problem/144
#_(defn oscilrate [val & fs]
  (lazy-seq
    (cons val
          (apply oscilrate (cons ((first fs) val) (concat (rest fs) [(first fs)]))))))

; Better Implementation: Use `reductions` and `cycle` to eliminate the above
; impl's (`lazy-seq` and `concat rest/first` calls.
(defn oscilrate
  "Write an oscillating iterate: a function that takes an initial value and a
  variable number of functions. It should return a lazy sequence of the
  functions applied to the value in order, restarting from the first function
  after it hits the end."
  [val & fs]
  (reductions
    (fn [v f]
      (f v))
    val
    (cycle fs)))

(comment
  (def __ oscilrate)
  (= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
  (= (take 5 (__ 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
  (= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3]))

; https://4clojure.oxal.org/#/problem/146
(defn tree-to-table []
  ; TODO: Solve.
)

(comment
  (def __ tree-to-table)
  (= (__ '{a {p 1, q 2}
           b {m 3, n 4}})
     '{[a p] 1, [a q] 2
       [b m] 3, [b n] 4})

  (= (__ '{[1] {a b c d}
           [2] {q r s t u v w x}})
     '{[[1] a] b, [[1] c] d,
       [[2] q] r, [[2] s] t,
       [[2] u] v, [[2] w] x})

  (= (__ '{m {1 [a b c] 3 nil}})
     '{[m 1] [a b c], [m 3] nil}))

; https://4clojure.oxal.org/#/problem/147
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
(defn p-t
  "Write a function that, for any given input vector of numbers, returns an
  infinite lazy sequence of vectors, where each next one is constructed from the
  previous following the rules used in Pascal's Triangle."
  [s]
  (iterate
    #(mapv +' (cons 0 %) (conj % 0))
    s))

(comment
  (def __ p-t)
  (= (second (__ [2 3 2])) [2 5 5 2])
  (= (take 5 (__ [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
  (= (take 2 (__ [3 1 2])) [[3 1 2] [3 4 3 2]])
  (= (take 100 (__ [2 4 2])) (rest (take 101 (__ [2 2])))))

; https://4clojure.oxal.org/#/problem/150
(defn palindromic-numbers
  "A palindromic number is a number that is the same when written forwards or
  backwards (e.g., 3, 99, 14341). Write a function which takes an integer n, as
  its only argument, and returns an increasing lazy sequence of all palindromic
  numbers that are not less than n."
  [n]
  )

(comment
  (def __ palindromic-numbers)
  (= (take 26 (__ 0))
     [0 1 2 3 4 5 6 7 8 9
      11 22 33 44 55 66 77 88 99
      101 111 121 131 141 151 161])

  (= (take 16 (__ 162))
     [171 181 191 202
      212 222 232 242
      252 262 272 282
      292 303 313 323])

  (= (take 6 (__ 1234550000))
     [1234554321 1234664321 1234774321
      1234884321 1234994321 1235005321])

  (= (first (__ (* 111111111 111111111)))
     (* 111111111 111111111)))

; https://4clojure.oxal.org/#/problem/153
(defn disjoint?
  "Given a set of sets, create a function which returns true if no two
  of those sets have any elements in common and false otherwise. "
  [s]
  (let [num-elems (->> (map count s) (reduce +))
        super-set (apply concat s)]
    (= num-elems (count super-set))))

; Better(?) shorter solution; requires clojure.set which 4clojure disallows.
; #(= (->> (map count %) (reduce +)) (count (apply clojure.set/union %)))

(comment
  (def __ #(= (->> (map count %)
                   (reduce +))
              (count (apply clojure.set/union %))))

  (= (__ #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
     true)

  (= (__ #{#{:a :b :c :d :e}
           #{:a :b :c :d}
           #{:a :b :c}
           #{:a :b}
           #{:a}})
     false)

  (= (__ #{#{[1 2 3] [4 5]}
           #{[1 2] [3 4 5]}
           #{[1] [2] 3 4 5}
           #{1 2 [3 4] [5]}})
     true)

  (= (__ #{#{'a 'b}
           #{'c 'd 'e}
           #{'f 'g 'h 'i}
           #{''a ''c ''f}})
     true)

  (= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
           #{#{:x :y :z} #{:x :y} #{:z} #{}}
           #{'[:x :y :z] [:x :y] [:z] [] {}}})
     false)

  (= (__ #{#{(= "true") false}
           #{:yes :no}
           #{(class 1) 0}
           #{(symbol "true") 'false}
           #{(keyword "yes") ::no}
           #{(class '1) (int \0)}})
     false)

  (= (__ (set [(set [distinct?])
               (set [#(-> %) #(-> %)])
               (set [#(-> %) #(-> %) #(-> %)])
               (set [#(-> %) #(-> %) #(-> %)])]))
     true)

  (= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
           #{'+ '* mapcat (comment mapcat)}
           #{(do) set contains? nil?}
           #{, , , #_, , empty?}})
     false))

; https://4clojure.oxal.org/#/problem/157
(defn index-seq
  "Transform a sequence into a sequence of pairs containing the original
  elements along with their index."
  [xs]
  (into [] (zipmap xs (range (count xs)))))

(comment
  (def __ index-seq)
  (= (__ [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
  (= (__ [0 1 3]) '((0 0) (1 1) (3 2)))
  (= (__ [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]]))

; https://4clojure.oxal.org/#/problem/158
(defn decurry
  "Write a function that accepts a curried function of unknown arity n.
  Return an equivalent function of n arguments."
  [curried-fn]
  (fn [& args]
    (reduce #(% %2) curried-fn args)))

(comment
  (def __ decurry)

  (= 10 ((__ (fn [a]
               (fn [b]
                 (fn [c]
                   (fn [d]
                     (+ a b c d))))))
         1 2 3 4))

  (= 24 ((__ (fn [a]
               (fn [b]
                 (fn [c]
                   (fn [d]
                     (* a b c d))))))
         1 2 3 4))

  (= 25 ((__ (fn [a]
               (fn [b]
                 (* a b))))
         5 5)))

; https://4clojure.oxal.org/#/problem/171
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

(comment
  (def __ intervals)
  (= (__ [1 2 3]) [[1 3]])
  (= (__ [10 9 8 1 2 3]) [[1 3] [8 10]])
  (= (__ [1 1 1 1 1 1 1]) [[1 1]])
  (= (__ []) [])
  (= (__ [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
     [[1 4] [6 6] [9 11] [13 17] [19 19]]))

; https://4clojure.oxal.org/#/problem/177
#_(defn balanced? [s]
  ; This implementation has a bug in that it only checks that num open
  ; matches num closed. Doesn't check ordering.
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

(defn balanced?
  " Write a function that takes in a string and returns truthy if all square [ ]
  round ( ) and curly { } brackets are properly paired and legally nested, or
  returns falsey otherwise."
  [s]
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
              ; If they didn't match, this string isn't balanced because tokens
              ; aren't properly paired.  Just push the closed token on, which
              ; will never get popped, as a sentinel.  When reduce finishes, we
              ; won't have an empty stack.
              (conj stack curr-char)))

          :else
          stack))
      []
      s)))

(comment
  (def __ balanced?)
  (__ "This string has no brackets.")

  (__ "class Test {
                          public static void main(String[] args) {
                            System.out.println(\"Hello world.\");
                          }
                        }")

  (not (__ "(start, end]"))
  (not (__ "())"))
  (not (__ "[ { ] } "))
  (__ "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")
  (not (__ "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))"))
  (not (__ "[")))

; https://4clojure.oxal.org/#/problem/195
(defn gen-parens
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

(comment
  (def __ gen-parens)
  (= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (__ n)) [0 1 2]))
  (= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (__ 3))
  (= 16796 (count (__ 10)))
  (= (nth (sort (filter #(.contains ^String % "(()()()())") (__ 9))) 6) "(((()()()())(())))")
  (= (nth (sort (__ 12)) 5000) "(((((()()()()()))))(()))"))
