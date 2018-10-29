(ns lambdaisland.li45-polymorphism
  (:require [clojure.xml]))

(conj #{} :x)
;; => #{:x}

(conj [] :x)
;; => [:x]

(conj {} [:x :y])
;; => {:x :y}


(defprotocol Datelike
  (year [d] "return the year (common era)")
  (month [d] "return the month (1-12)")
  (day [d] "return the day of the month (1-31)"))

(defn days-since-new-year [datelike]
  (let [y (year datelike)
        m (month datelike)
        d (day datelike)
        leap? (and (= 0 (mod y 4))
                   (or (not= 0 (mod y 100))
                       (= 0 (mod y 400))))]
    (cond-> (apply + (take m [d 31 28 31 30 31 30 31 31 30 31 30 31]))
      (and leap? (> m 2)) inc)))

(defrecord NaiveDate [y m d]
  Datelike
  (year [this] y)
  (month [this] m)
  (day [this] d))

(def d (->NaiveDate 2018 10 22))

(year d)
;; => 2018
(month d)
;; => 10
(day d)
;; => 22

(days-since-new-year d)
;; => 295

(extend-type java.util.Date
  Datelike
  (year [this] (+ 1900 (.getYear this)))
  (month [this] (inc (.getMonth this)))
  (day [this] (.getDate this)))

(year (java.util.Date.))
;; => 2018

(month (java.util.Date.))
;; => 10

(day (java.util.Date.))
;; => 26

(def s (java.sql.Date. 1540240505000))

(year s)
;; => 2018

(month s)
;; => 10

(day s)
;; => 22

(supers java.sql.Date)
;; => #{java.io.Serializable java.util.Date java.lang.Cloneable java.lang.Object
;;      java.lang.Comparable}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:item/type :door, :door/open? true}
{:item/type :wall}
{:item/type :magical-potion}

(defn obstacle? [item]
  (case (:item/type item)
    :door (not (:door/open? item))
    :wall false
    true))

(defmulti obstacle?
  "Does this item block the player's path?"
  {:added "1.0"}
  :item/type
  :default ::unknown-item-type
  ;;:hierarchy ,,,
  )

(defmethod obstacle? :door [item]
  (not (:door/open? item)))

(defmethod obstacle? :wall [item]
  true)

(defmethod obstacle? ::unknown-item-type [item]
  ;; (log/warn "Missing implementation of (obstacle? " (:item/type item) ")")
  false)

(comment
  (ns-unmap *ns* 'obstacle?)
  (def obstacle? nil)
  (reloaded.repl/reset)
  ;;or
  (integrant.repl/reset)
  )

(obstacle? {:item/type :potion})
;; => false

(make-hierarchy)
;; => {:parents {}, :descendants {}, :ancestors {}}

(def my-hierarchy (-> (make-hierarchy)
                      (derive :gate :door)
                      (derive :rock-face :wall)
                      (derive :city-gate :gate)))

my-hierarchy
;; => {:parents {:gate #{:door}, :rock-face #{:wall}, :city-gate #{:gate}},
;;     :ancestors {:gate #{:door}, :rock-face #{:wall}, :city-gate #{:door :gate}},
;;     :descendants
;;     {:door #{:city-gate :gate}, :wall #{:rock-face}, :gate #{:city-gate}}}

(isa? my-hierarchy :gate :door)
;; => true

(isa? my-hierarchy :city-gate :door)
;; => true

(isa? my-hierarchy java.sql.Date java.util.Date)
;; => true

(derive my-hierarchy `foo `bar)
(derive my-hierarchy java.util.Date ::datelike)

(defrecord Fence [])

(derive my-hierarchy Fence :wall)


(alter-var-root #'my-hierarchy derive Fence :wall)

(isa? my-hierarchy Fence :wall)
;; => true

(isa? my-hierarchy 5 5)
;; => true

(isa? my-hierarchy {:x :y} {:x :y})
;; => true

(isa? my-hierarchy [:city-gate :rock-face] [:door :wall])
;; => true

(isa? my-hierarchy
      [[:city-gate :rock-face] {:x :y} 5]
      [[:door :wall] {:x :y} 5])
;; => true

@#'clojure.core/global-hierarchy
;; => {:parents {}, :descendants {}, :ancestors {}}

(isa? 5 5)
;; => true

(isa? :gate :door)
;; => false

(derive ::gate ::door)
;; => nil

(isa? ::gate ::door)
;; => true

(comment
  (derive :gate :door)
  ;;=> Exception
  )

(underive ::gate ::door)
;; => nil

(isa? ::gate ::door)
;; => false





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti obstacle? :item/type)

(defmethod obstacle? :default [item]
  false)

(defmethod obstacle? ::door [item]
  (not (:door/open? item)))

(defmethod obstacle? ::wall [item]
  true)


(derive ::gate ::door)
;; => nil

(obstacle? {:item/type ::gate :door/open? true})
;; => false

(obstacle? {:item/type ::gate :door/open? false})
;; => true

(def obstacle-hierarchy (make-hierarchy))

(defn obstacle-derive! [child parent]
  (alter-var-root #'obstacle-hierarchy derive child parent))

#_(def obstacle? nil)

(defmulti obstacle? :item/type :hierarchy #'obstacle-hierarchy)

(obstacle-derive! ::gate ::door)

;; - dispatch on any possible attribute
;; - hierarchies as first class citizens
;; - separate hierarchies for each method














;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; method overloading

;; function attack(Player p, Zombie z) { }
;; function attack(Player p, Zombie z) { }
;; function attack(Player p, Creature c1, Creature c2) { }
;; function attack(Zombie z, Skeleton s) { }

(defn attack
  ([attacker target] ,,,)
  ([attacker target1 target2] ,,,))


(defrecord Player [x y hp])
(defrecord Zombie [x y hp desc])
(defrecord Skeleton [x y hp desc])

(derive Zombie ::creature)
(derive Skeleton ::creature)


(defrecord Game [player creatures])

(defmulti into-game (fn [& args] (mapv type args)))

(defmethod into-game [Game Player] [game player]
  (assoc game :player player))

(defmethod into-game [Game ::creature] [game c]
  (update game :creatures conj c))

(def empty-game (map->Game {:creatures ^{:type ::creature-list} []}))

(def game
  (reduce into-game
          empty-game
          [(->Player 10 15 99)
           (->Zombie 1 1 15 "shambler")
           (->Zombie 20 10 15 "fastzom")
           (->Skeleton 25 7 10 "lil bones")]))

(defmulti to-xml type)

(defmethod to-xml Game [{:keys [player creatures]}]
  {:tag :game
   :content [(to-xml player)
             (to-xml creatures)]})

(defmethod to-xml Player [{:keys [x y hp]}]
  {:tag :player
   :attrs {:x x
           :y y
           :hp hp}})

(defmethod to-xml ::creature [creature]
  {:tag (keyword (.getSimpleName (class creature)))
   :attrs (into {} creature)})

(defmethod to-xml ::creature-list [l]
  {:tag :creatures
   :content (map to-xml l)})

(clojure.xml/emit
 (to-xml game))


(class [])
;; => clojure.lang.PersistentVector

(type [])
;; => clojure.lang.PersistentVector

(type ^{:type ::creature-list} [])
;; => :lambdaisland.li45-polymorphism/creature-list







