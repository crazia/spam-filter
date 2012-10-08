(ns spam-filter.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


;; 괜찮나 (ham) 안 괜찮나 (spam) 의 기준점을 나누기 
(def max-ham-score 0.4)
(def min-spam-score 0.6)


;; Global 저장 객체들 선언 

(def database (ref {}))
(def total-spams (ref 0))
(def total-hams (ref 0))


(defstruct word-feature :word :spam-count :ham-count)

;; 주어진 점수에 따라서 스팸인지 아닌지를 최종 결정하는 함수 
(defn classification [score]
  (cond
    (<= score max-ham-score) 'ham
    (>= score min-spam-score) 'spam
    true 'unsure))


;; database 에 주어진 작업을 하는 내용 
;; (defn intern-feature [word]
;;   (dosync
;;    (alter database merge {word (struct word-feature word 0 0)})))

;; (defn intern-feature [word]
;;   (dosync
;;    (alter database merge {word (struct word-feature word 0 0)}))
;;   (database word))

(defn intern-feature [word]
  (let [db (database word)]
    (if (nil? db)
      (do       
        (dosync
         (alter database merge {word (struct word-feature word 0 0)}))
        (database word))
      db)))

;; 크기대로 (영문은 3글자 이상, 한글은 2글자 이상) 글자를 나누는 작업
;; 중복된 글자들은 한개로 처리 한다. 
(defn extract-words [text]
  (distinct (re-seq #"[a-zA-Z]{3,}|[가-힣]{2,}" text)))



;; 주어진 문장을 전부 쪼개서 데이타베이스에 저장하기 
(defn extract-features [text]
  (map intern-feature (extract-words text)))


(defn increment-feature-count [feature type]
  (assoc feature type (inc (feature type))))

;;
;; (defn increment-count [word type]
;;   (let [db ((intern-feature word) word) ]
;;     (dosync
;;      (alter database merge {word (increment-feature-count db type)}))))

(defn increment-count [word type]
  (let [db (intern-feature word)]
    (dosync
     (alter database merge {word (increment-feature-count db type)}))))



(defn increment-total-count [type]
  (case type
    :ham-count (dosync (alter total-hams inc))
    :spam-count (dosync (alter total-spams inc))))

(defn clear-database! []
  (dosync (ref-set database {}) ;; not set but map 
          (ref-set total-spams 0)
          (ref-set total-hams 0)))


(defn train [text type]
  (doseq [ feature (extract-features text) ]
    (increment-count (:word feature) type))
  (increment-total-count type))

; broken out into separate functions, unlike PCL
;; (defn spam-frequency [feature]
;;   (/ (feature :spam-count) (max 1 @total-spams)))
;; (defn ham-frequency [feature]
;;   (/ (feature :ham-count) (max 1 @total-hams)))

;; (defn spam-probability [feature]
;;   (/ (spam-frequency feature) 
;;      (+ (spam-frequency feature) (ham-frequency feature))))

(defn spam-probability [feature]
  (let [spam-frequency (/ (feature :spam-count) (max 1 @total-spams))
        ham-frequency (/ (feature :ham-count) (max 1 @total-hams)) ]
    (/ spam-frequency (+ spam-frequency ham-frequency))))


(defn bayesian-spam-probability
  ([feature] 
     (bayesian-spam-probability feature 1/2))
  ([feature assumed-probability] 
     (bayesian-spam-probability feature assumed-probability 1))
  ([feature assumed-probability weight]
     (let [basic-probability (spam-probability feature)
	   data-points (+ (:spam-count feature) (:ham-count feature))]
       (/ (+ (* weight assumed-probability)
	     (* data-points basic-probability))
	  (+ weight data-points)))))



;; (defn score [features]
;;   (let [spam-probs (map bayesian-spam-probability features)
;;         ham-probs (map #(- 1 %1) spam-probs)
;;         num (count features)
;;         h (- 1 (fisher spam-probs num))
;;         s (- 1 (fisher ham-probs num))]
;;      (/ (+ (- 1 h) s) 2)))


