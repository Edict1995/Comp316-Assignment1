(ns assignment1.core
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(defn count-list
  [list item]
  (reduce (fn [acc entity]
    (if(= item entity)
      (+ acc 1) acc))
          0
          list) 
  )



(defn create-multiset
  [list]
  (loop [coll {} list list]
    (if(empty? list)
      coll
      (if(contains? coll (first list))
        (recur coll (rest list))
        (recur (into coll {(first list) (count-list list (first list))}) (rest list))))))

(defn size-multiset
  [multiset]
  
  (reduce + (vals multiset))
  )

(defn intersect-multisets
  [ms1 m2]
  (loop [coll {} m1 ms1]
    (if(empty? m1)
      coll
      (if (contains? m2 (key (first m1)))
        (recur (into coll (hash-map  (key (first m1))
                                     (if (<= (val(first m1)) (get m2 (key (first m1))))
                                       (val (first m1))
                                       (get m2 (key (first m1))))))
               (rest m1))
        (recur coll (rest m1))))))

(intersect-multisets {"a" 1 "b" 2 "c" 3} {"b" 1 "c" 2})

(defn  part1  []
  (let [a (create-multiset
           '("ab" "cd" "ef" "ab" "fg" "ef" "ab" "dd"))
        b (create-multiset
           '("zz" "ef" "fg" "ef" "ab" "ab" "ef" "ef"))
        c (intersect-multisets a b)]
    (println "a = " a ", size = " (size-multiset a))
    (println "b = " b ", size = " (size-multiset b))
    (println "c = " c ", size = " (size-multiset c))))

(require '[clojure.string :as str])

(defn create-multiset-from-text-file
  [filename]
  (-> filename slurp (str/split #"\s+") create-multiset))

(create-multiset-from-text-file "/home/sam/assignment1/unix-text-files/positive-words.txt")


(defn analyse-sentiment
  [pos-ms neg-ms filename]
  (let[file (create-multiset-from-text-file filename)
       pos (count (intersect-multisets file pos-ms))
       neg (count (intersect-multisets file neg-ms))
       result (- pos neg)]
    (println "Positive Score " pos)
    (println "Negative Score " neg)
    (println "Final Sentiment " result)
    ))

(defn  part2
  []
  (let [pos (create-multiset-from-text-file
             "/home/sam/assignment1/unix-text-files/positive-words.txt")
        neg (create-multiset-from-text-file
             "/home/sam/assignment1/unix-text-files/negative-words.txt")
        ]
    (println "size of pos  dictionary = " (size-multiset  pos))
    (println "size of neg  dictionary = " (size-multiset  neg))
    (analyse-sentiment pos neg "/home/sam/assignment1/unix-text-files/pos1.txt")
    (analyse-sentiment  pos neg "/home/sam/assignment1/unix-text-files/pos3.txt")
    (analyse-sentiment  pos neg "/home/sam/assignment1/unix-text-files/pos3.txt")
    (analyse-sentiment  pos neg "/home/sam/assignment1/unix-text-files/neg1.txt")
    (analyse-sentiment  pos neg "/home/sam/assignment1/unix-text-files/neg2.txt")
    (analyse-sentiment  pos neg "/home/sam/assignment1/unix-text-files/neg3.txt")
    )
  )
(part2)
