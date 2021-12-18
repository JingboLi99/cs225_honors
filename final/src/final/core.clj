;; (ns cs-225-honors-project.core
;;   (:gen-class))

;; (defn -main
;;   "I don't do a whole lot ... yet."
;;   [& args]
;;   (println "Hello, World!"))

;; Code from Interaction folder on CS 225 Honors Github 
;; Note : Some functions might not be useful to the whole project and might need to be deleted before submission

(ns interaction.core 
  (:require [clojure.string :as str]))
(require '[clojure.string :as str])
(require '[clojure.core.match :refer [match]])


;; (def x 10)
(def pokespace {
    0 [1 5] 1 [0 6 2] 2 [1 3] 3 [4 2 8] 4 [3 9] 5 [6 0 10]
    6 [1 7 5] 7 [8 6 12] 8 [3 7] 9 [4 14] 10 [11 5]
    11 [10 16] 12 [7 13] 13 [12 18] 14 [9 19]
    15 [16] 16 [11 15 17 21] 17 [16 18] 18 [13 17 19 23]
    19 [11 18 24] 20 [21] 21 [16 20 22] 22 [21 23] 23 [18 22] 24 [19] })
;; # Interaction
;; A simple parser that can remember facts.
;;

;; # Actions
;;
;; Let an *action* be a function with a specific
;; interface: It will take in a *state* followed by
;; a sequence of arguments.  It will return a vector
;; consisting of a new state and a response string.
;;
;; Here is a simple example, a post-increment function.
;; Assume the state is a hash-map with a key :vars
;; and a value containing a hash of the variables and
;; their values.


(defn post-increment
  "Action: post-increment
   Returns the value of a variable from the state and increments it."
  [state var]
  (if-let [val (get-in state [:vars var])]
    [(update-in state [:vars var] inc) val]
    [(assoc-in state [:vars var] 1) 0]))

;; <pre><code>
;; interaction.core=> (post-increment {:vars {:x 10}} :x)
;; [{:vars {:x 11}} 10]
;; </code></pre>

;; ## Your work
;;
;; Fill in the code for these functions.
;;

(defn rand-pokemon-placement
  [maximum leaving]
  (let [place (rand-int maximum)]
    (if (leaving place) (rand-pokemon-placement maximum leaving) place)
    )
  )

(defn lookup-var
  "Given a state and a variable name, return the value of the variable
  if it has been defined, otherwise return 0."
  [state var]
  (if-let [val (get-in state var )]
    [state val]
    [state 0])
  )

;; <pre><code>
;; interaction.core=> (lookup-var {:vars {:x 10}} :x)
;; [{:vars {:x 10}} 10]
;; </code></pre>

(defn set-plus
  "Action: set-plus.  Set var = e1 + e2, return the sum as a result."
  [state var e1 e2]
  (let [v1 (if (number? e1) e1 (lookup-var state e1))
        v2 (if (number? e2) e2 (lookup-var state e2))
        s (+ v1 v2)]
    [(assoc-in state var s) s]
    )
  )

;; <pre><code>
;; interaction.core=> (set-plus {:vars {:x 10}} :y :x 20)
;; [{:vars {:x 10 :y 30}} 30]
;; </code></pre>

(defn set-var
  "Action: set-var. Set var = e1.  Return the new value as a result."
  [state var e1]
  ( let [temp1 (if (number? e1) e1 (lookup-var state e1))]
    [(assoc-in state var temp1)]
   )
)  

;; <pre><code>
;; interaction.core=> (set-var {:vars {:x 10}} :y :x)
;; [{:vars {:x 10 :y 10}} 10]
;; </code></pre>

(defn there-is-a
  "Action: there-is-a.  Remember that an object obj exists.
  Returns \"There is a obj\" as a result."
  [state object]
  (if (get-in state [:objects object])
    [state "I kinda already knew that."]
    [(assoc-in state [:objects object] #{}) (str "There is a " (name object) ".")])
  )

;; <pre><code>
;; interaction.core=> (there-is-a {:vars {:x 10}} :shoe)
;; [{:vars {:x 10 :y 10}
;;   :objects {:shoe #{}}} "There is a shoe."]
;; </code></pre>

(defn the-obj-is
  "Action: there-obj-a.  Remember an adjective that applies to an object.
  Returns \"The obj is adj\" as a result."
  [state category object adj]
  (if (get-in state [category object])
    (do 
     (update-in state [category object] (fn [x] (conj x adj)))
     (println "Your personal pokemon is " (name adj) ".") 
     state
    )
    ()
   )
  )

;; <pre><code>
;;
;; interaction.core=> (the-obj-is {:vars {:x 10} :objects {:shoe []}} :shoe :blue)
;; [{:vars {:x 10} :objects {:shoe [:blue]}} "The shoe is blue."]
;; </code></pre>



(defn describe-obj
  "Describe the given object \"The obj is adj\" if it exists in state . If not, return \"There is no obj\""
  [state category object attribute]
  (if-let [adj (get-in state [category object attribute])]
    [str "The obj is" (adj 1) ". The obj is also " (adj 2)]
    [str "There is no" object]
    )
  )
;; <pre><code>
;; interaction.core=> (describe-obj  {:vars {:x 10} :objects {:shoe [:blue :big]}} :shoe)
;; [{:vars {:x 10}, :objects {:shoe [:blue :big]}} "The shoe is blue." "The shoe is big."]
;; </code></pre>


(defn forget-obj
  "Delete the given object and return \"What obj?\""
  [state object]
  (if (get-in state [:objects object]) 
    [ (dissoc state [:objects object]) state (str "What obj?") ]
    (state)
    )
  )
;; <pre><code>
;; interaction.core=> (forget-obj {:objects {:show [:exciting]}} :show)
;; [{:objects {}} "What show?"]
;; </code></pre>


;; # Action Environment
;;
;; The runtime environment is a vector of the form
;;
;; ``` [ phrase action phrase action ...]```
;;
;; The "phrase" is a canonicalized vector of keywords.
;; (Remember that a keyword is a symbol starting with a colon, like :name.)
;; The `@` character will represent a variable.

;; (def initial-env [  [:postinc "@"] post-increment  ])  ;; add your other functions here


(defn initial-env []
  (let [ 
        charmainder (rand-pokemon-placement 24 #{0}) 
        squirtle (rand-pokemon-placement 24 #{charmainder, 0})
        bulbasur (rand-pokemon-placement 24 #{charmainder, squirtle, 0})
        darkrai (rand-pokemon-placement 24 #{charmainder, squirtle, bulbasur, 0})
        
       
        charmainderlevel (rand-pokemon-placement 3 #{})
        squirtlelevel (rand-pokemon-placement 3 #{})
        bulbasurlevel (rand-pokemon-placement 3 #{})
        darkrailevel (rand-pokemon-placement 3 #{})
        
        
       ]
     { :pokemons {:charmainder  {:loc charmainder :level charmainderlevel :size "big"} 
                      :squirtle {:loc squirtle :level squirtlelevel :size "small" } 
                      :bulbasur {:loc bulbasur :level bulbasurlevel :size "medium" }
                      :darkrai {:loc darkrai :level darkrailevel :size "small" }
                      }
       :masterPosition 0 
       :masterStatus :alive
       :wins 0
     }
  )
)  ;; add your other functions here

;; # Parsing
;;
;; This is mostly code from the lecture.



;; (defn canonicalize
;;   "Given an input string, strip out whitespaces, lowercase all words, and convert to a vector of keywords."
;;   [input]
;;   )

;; <pre><code>
;; interaction.core> (canonicalize "The shoe is blue.")
;; [:the :shoe :is :blue]
;; </code></pre>

;; (defn react
;;   "Given a state and a canonicalized input vector, search for a matching phrase and call its corresponding action.
;;   If there is no match, return the original state and result \"I don't know what you mean.\""
;;   [state input-vector]
;;   (match input-vector
;;     [:there :is :a obj] (there-is-a state obj)
;;     [:name obj1 obj2 :size size1 size2] ( do
;;                                          (the-obj-is  state :pokemons :personalPoke1 obj1)
;;                                          (the-obj-is  state :pokemons :personalPoke1 size1)
;;                                          (update-in state [:pokemons :personalPoke1] (fn [x] (conj x (rand-pokemon-placement 3 #{}) )))
;;                                          (the-obj-is  state :pokemons :personalPoke2 obj2)
;;                                          (the-obj-is  state :pokemons :personalPoke2 size2)
;;                                          (update-in state [:pokemons :personalPoke2] (fn [x] (conj x (rand-pokemon-placement 3 #{}) ))))
;;     :else (println "I don't know what you mean.")
;;     )
;;   state
;; )



(defn playerPokemons 
  [obj1 obj2 size1 size2] (def myPokeState { 
                           :personalPoke1 {:name obj1 :level (rand-pokemon-placement 3 #{}) :size size1}
                           :personalPoke2 {:name obj2 :level (rand-pokemon-placement 3 #{}) :size size2}
                                  } )
  (println "Your first personal pokemon is polio " (name obj1) 
           ". It's size is " (name size1) 
           ". It's level is " (get-in myPokeState [ :personalPoke1 :level]) ".")
  (println "Your second personal pokemon is " (name obj2) 
           ". It's size is " (name size2) 
           ". It's level is " (get-in myPokeState [ :personalPoke2 :level]) ".")
  
  )

(defn react
  "Given a state and a canonicalized input vector, search for a matching phrase and call its corresponding action.
  If there is no match, return the original state and result \"I don't know what you mean.\""
  [state input-vector]
  (match input-vector
    [:there :is :a obj] (there-is-a state obj)
    [:name obj1 obj2 :size size1 size2] (playerPokemons obj1 obj2 size1 size2 )
    :else (do 
            (println "I don't know what you mean.")
             state 
              )
    )
)


(defn shuffleverything [state] 
  (let [ pos (rand-pokemon-placement 24 #{(get-in state [:pokemons :bulbasur :loc]) 
                                          (get-in state [:pokemons :darkrai :loc]) 
                                          (get-in state [:pokemons :charmainder :loc]) 
                                          (get-in state [:pokemons :squirtle :loc])})] 
   (assoc state :masterPosition pos)
  )
)

(defn declareWinner [state]
  (println "This is check  " state)
   (cond (== (get-in state [:wins]) 2) 
        (do
           (println "Congratulations!! You have won the game" )
           (update-in state [:masterStatus] :won)
        )
        (== (get-in state [:wins]) -2 )
       (do
         (println "Sorry!! You have lost the game" )
        (update-in state [:masterStatus] :lose)
       )
       :else state
       
  )
)

(defn updateWins [state won wildpokemon]
  (if (= won wildpokemon)
  (do 
    (println "Oh no, the wild pokemon " (name wildpokemon) " wins the fight with your pokemon" ) 
    (declareWinner (update-in state [:wins] (fn [x] (- x 1))) ) 
    ) 
  (do
    (println "Congratulations! Your pokemon wins the fight with " (name wildpokemon)
             " . You have captured the wild pokemon.")
    ( declareWinner (update-in state [:wins] (fn [x] (+ x 1))) )
    )
   )
     
  )


(defn abs [n] (max n (- n)))

(defn fight [state pokemon wildpokemon myPokeState]
  (let [win []]
   (println "This is myPokeState" (get-in myPokeState [:personalPoke1 :name])) 
  (if (== (get-in state [:pokemons wildpokemon :level]) (get-in myPokeState [pokemon :level])  )
    (if (== (rand-int 8) (or 0 1 2 3))
     (updateWins state ((conj win pokemon) 0) wildpokemon )
      (updateWins state ((conj win wildpokemon) 0) wildpokemon )
      )
    (if (== (abs (compare (get-in state [:pokemons wildpokemon :level]) (get-in myPokeState [pokemon :level]) ))  1)
     (if ( and (== (rand-int 8) (or 0 1 2)) (> (get-in state [:pokemons wildpokemon :level]) (get-in myPokeState [pokemon :level]) ))
      (updateWins state ((conj win pokemon) 0) wildpokemon )
      (updateWins state ((conj win wildpokemon) 0) wildpokemon )
      )
      (if ( and (== (rand-int 8) (or 0 1 )) (> (get-in state [:pokemons wildpokemon :level]) (get-in myPokeState [pokemon :level]) ))
        (updateWins state ((conj win pokemon) 0) wildpokemon )
        (updateWins state ((conj win wildpokemon) 0) wildpokemon )
        )
    )
  )
 )
)
  
 



(defn direction 
  [loc] 
  (let [possiblepaths []]
    (for [ x (get-in pokespace [loc])]
     (if (== x (+ loc 1))
       ((conj possiblepaths "Right " ) 0)
       (if (== x (+ loc 5))
         ((conj possiblepaths "Down " ) 0)
         (if (== x (- loc 1))
           ((conj possiblepaths "Left " ) 0)
           (if (== x (- loc 5))
             ((conj possiblepaths "Up " ) 0)
             ())
         )
       )
     )
    )
  )
)

(defn numerical-direction 
  [state dir] 
  (let [ans []]
   (if ( == (compare (str dir) "R") 0 )
       ( (conj ans (+ (get-in state [:masterPosition]) 1)) 0)
       (if ( == (compare (str dir) "D") 0 )
         ( (conj ans (+ (get-in state [:masterPosition]) 5)) 0)
         (if ( == (compare (str dir) "L") 0 )
           ( (conj ans (- (get-in state [:masterPosition]) 1)) 0)
           (if ( == (compare (str dir) "U") 0 )
             ( (conj ans (- (get-in state [:masterPosition]) 5)) 0)
             ())
         )
       )
     )
)
) 


(defn lookingAround [state]
    (let [loc (get state :masterPosition)]
        (if (some #{(get-in state [:pokemons :charmainder :loc]) 
                    (get-in state [:pokemons :squirtle :loc]) 
                    (get-in state [:pokemons :bulbasur :loc]) 
                    (get-in state [:pokemons :darkrai :loc])} 
                  (pokespace loc))
            (println "You hear some noise. A pokemon is nearby. ") (println "There is no pokemon nearby."))
        state
      ) 
 )


(defn evaluateAction [state personalpokemons]
  (let [curloc (state :masterPosition) ]
    (cond (== curloc (get-in state [:pokemons :charmainder :loc] ))
      (do (println "You found a wild charmainder of level " 
                   (get-in state [:pokemons :charmainder :level]) 
                   ", do you want to [F]ight or [R]un for the hills? " )
                   (let [selection (read-line)] 
                     (cond (= selection "F") 
                     (do (println "Choose your pokemon to fight: 1." 
                                  (get-in personalpokemons [:personalPoke1 :name]) 
                                  " 2." (get-in personalpokemons [:personalPoke2 :name]))
                       (let [pokechoice (read-line)]
                         (if (= pokechoice "1" )
                         (fight state :personalPoke1 :charmainder personalpokemons)
                         (fight state :personalPoke2 :charmainder personalpokemons)
                         )
                      )
                     )    
                     (= selection "R") (shuffleverything state) 
                     )     
                    )
      )   
      (== curloc (get-in state [:pokemons :squirtle :loc] ))
      (do (println "You found a wild squirtle of level " 
                   (get-in state [:pokemons :squirtle :level]) 
                   ", do you want to [F]ight or [R]un for the hills? " )
                   (let [selection (read-line)] 
                     (cond (= selection "F") 
                     (do (println "Choose your pokemon to fight: 1." 
                                 (get-in personalpokemons [:personalPoke1 :name]) 
                                  " 2." (get-in personalpokemons [:personalPoke2 :name]))
                       (let [pokechoice (read-line)]
                         (if (= pokechoice "1" )
                         (fight state :personalPoke1 :squirtle personalpokemons)
                         (fight state :personalPoke2 :squirtle personalpokemons)
                         )
                      )
                     )    
                     (= selection "R") (shuffleverything state) 
                     )     
                     )
      ) 
      (== curloc (get-in state [:pokemons :bulbasur :loc] ))
      (do (println "You found a wild bulbasur of level " 
                   (get-in state [:pokemons :bulbasur :level]) 
                   ", do you want to [F]ight or [R]un for the hills? " )
                   (let [selection (read-line)] 
                     (cond (= selection "F") 
                     (do (println "Choose your pokemon to fight: 1." 
                                  (get-in personalpokemons [:personalPoke1 :name]) 
                                  " 2." (get-in personalpokemons [:personalPoke2 :name]))
                       (let [pokechoice (read-line)]
                         (if (= pokechoice "1" )
                         (fight state :personalPoke1 :bulbasur personalpokemons)
                         (fight state :personalPoke2 :bulbasur personalpokemons)
                         )
                      )
                     )    
                     (= selection "R") (shuffleverything state) 
                     )     
                     )
      )
      (== curloc (get-in state [:pokemons :darkrai :loc] ))
      (do (println "You found a wild darkrai of level " 
                   (get-in state [:pokemons :darkrai :level]) 
                   ", do you want to [F]ight or [R]un for the hills? " )
                   (let [selection (read-line)] 
                     (cond (= selection "F") 
                     (do (println "Choose your pokemon to fight: 1." 
                                  (get-in personalpokemons [:personalPoke1 :name]) 
                                  " 2." (get-in personalpokemons [:personalPoke2 :name]))
                       (let [pokechoice (read-line)]
                         (if (= pokechoice "1" )
                         (fight state :personalPoke1 :darkrai personalpokemons)
                         (fight state :personalPoke2 :darkrai personalpokemons)
                         )
                      )
                     )    
                     (= selection "R") (shuffleverything state) 
                     )     
                     )
      )
      :else state 
  )
 )
)




;; <pre><code>
;; interaction.core> (react {:vars {:x 10} :runtime initial-env} [:postinc :x])
;; [ {:vars {:x 11} :runtime { ... omitted for space ... }}  10]
;; </code></pre>

(defn repl
  "Start a REPL using the given environment.  The :runtime key should map to the action environment.
  Prints out the result and loops the state for another round.  Quits when you say bye.
  You may need (flush) to print out '>' without a newline "
  [env]
  
   (loop [state env]
    (println "You are in the Poke Space")
    (println "Before starting the game, you can create two pokemons of your choice. Write the name and size of your pokemon as - Name Ayush, Ash , Size Big, Small")
    (let [input
          (map keyword (map str/lower-case (str/split (read-line) #"(\W+|[.?!])")))]
          (if (= (react env (vec input)) env) 
            (recur state) 
            ())
    )
   )
  

  (loop [state env]
    (println "The current state is:" state)
    (if (= (:masterStatus state) :alive)
      (do
       (println "The world in this space is filled with many pokemons. "
                "You need to capture two pokemons to win the game. ")
    
        (println state "          What do you want to do? ([S]earch/[Q]uit)" ) ;; remove printing state after debugging 
        (let [selection (read-line)] 
          (cond (= selection "S") 
                (do  
                  (lookingAround state)
                  (println " You can move in the given directions: " (direction (-> :masterPosition state))
                  " What do you want to do? (For Right - R / Left - L / Up - U / Down - D)")
                  (let [ dir (read-string (read-line))]
                          (if (some #{ (numerical-direction state dir) } (-> :masterPosition state pokespace))
                              (recur (evaluateAction (assoc state :masterPosition (numerical-direction state dir) ) myPokeState )) 
                              (do (println "You can't go there.")
                                  (recur state)))))
                (= selection "Q") (println "Thanks for playing Capture the Pokemons!")
                :else (do 
                        (println "I don't know what you mean. ")
                        (recur state)  
                          )
                  )
          )
       
      )
    (println "Game over.") ) 
  ) 
)
  


;; <pre><code>
;; interaction.core=> (repl initial-env)
;; Welcome!  Let's talk.
;; > there is a spoon.
;; There is a spoon.
;; > the spoon is blue.
;; The spoon is blue.
;; > the spoon is big.
;; The spoon is big.
;; > describe the spoon.
;; The spoon is blue.
;; The spoon is big.
;; > forget the spoon.
;; What spoon?
;; > describe the spoon.
;; There is no spoon.
;; > bye
;; nil
;; interaction.core=> 
;; </code></pre>

(defn main
  "Start the REPL with the initial environment."
  []
  (repl (initial-env)
  )
)
