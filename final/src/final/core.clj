
(ns final.core 
  (:require [clojure.string :as str]))
(require '[clojure.string :as str])
(require '[clojure.core.match :refer [match]])

(def pokespace {
    0 [1 5] 1 [0 6 2] 2 [1 3] 3 [4 2 8] 4 [3 9] 5 [6 0 10]
    6 [1 7 5] 7 [8 6 12] 8 [3 7] 9 [4 14] 10 [11 5]
    11 [10 16] 12 [7 13] 13 [12 18] 14 [9 19]
    15 [16] 16 [11 15 17 21] 17 [16 18] 18 [13 17 19 23]
    19 [11 18 24] 20 [21] 21 [16 20 22] 22 [21 23] 23 [18 22] 24 [19] })


(defn rand-stuff-placement
  [maximum leaving]
  (let [place (rand-int maximum)]
    (if (leaving place) (rand-stuff-placement maximum leaving) place)
    )
  )

(defn rand-poke-placement
  [allowed]
  (let [pos (rand-int 6)]
    (nth allowed pos) ;;choose the element at index [pos] from vector [allowed]
  )
)

(defn initial-env []
  (let [
        ;;hardcoded env areas 
        fire_area (vector 0 1 2 5 6 7)
        water_area (vector 3 4 8 9 13 14)
        nature_area (vector 18 19 21 22 23 24)
        dark_area (vector 10 11 15 16 17 20)


        charmainder (rand-poke-placement fire_area) 
        squirtle (rand-poke-placement water_area)
        bulbasur (rand-poke-placement nature_area)
        darkrai (rand-poke-placement dark_area)
        
        personalLevel1 (rand-stuff-placement 3 #{})
        personalLevel2 (rand-stuff-placement 3 #{})
        
        charmainderlevel (rand-stuff-placement 3 #{})
        squirtlelevel (rand-stuff-placement 3 #{})
        bulbasurlevel (rand-stuff-placement 3 #{})
        darkrailevel (rand-stuff-placement 3 #{})
        ;;added here
        bluePotPos (rand-stuff-placement 25 #{charmainder, squirtle, bulbasur, 12}) ;;potion placement will generate a vector of 2 positions 
        redPotPos (rand-stuff-placement 25 #{bluePotPos, charmainder, squirtle, bulbasur, 12}) ;;potion placement will generate a vector of 2 positions 
        
       ]
     { :pokemons {:charmainder  {:loc charmainder :level charmainderlevel :size "big"} 
                      :squirtle {:loc squirtle :level squirtlelevel :size "small" } 
                      :bulbasur {:loc bulbasur :level bulbasurlevel :size "medium" }
                      :darkrai {:loc darkrai :level darkrailevel :size "small" }
                      }
       :masterPosition 12 ;;added here changed masterPosition to 12 
       :masterStatus :alive
       :wins 0 
       ;;added here
       :potion {:blue {:loc bluePotPos :exist true}
                :red {:loc redPotPos :exist true}
                }
       :personalPokemonsLevels {
                                 :personalPoke1 {:level personalLevel1}
                                 :personalPoke2 {:level personalLevel2}
                                 
       }
      :fire_area fire_area
      :water_area water_area
      :nature_area nature_area
      :dark_area dark_area
     }
  )
)


(defn playerPokemons 
  [state obj1 obj2 size1 size2] ( def myPokeState { 
                           :personalPoke1 {:name obj1 :size size1}
                           :personalPoke2 {:name obj2 :size size2}
                                  } )
  (println "Your first personal pokemon is " (name obj1) 
           ". It's size is " (name size1) 
           ". It's level is " (get-in state [:personalPokemonsLevels :personalPoke1 :level])".")
  (println "Your second personal pokemon is " (name obj2) 
           ". It's size is " (name size2) 
           ". It's level is "  (get-in state [:personalPokemonsLevels :personalPoke2 :level]) ".")
  (println "  ")
  (println "The world in this space is filled with many pokemons. You need to capture pokemons to win the game. ")
  
  )

(defn react
  "Given a state and a canonicalized input vector, search for a matching phrase and call its corresponding action.
  If there is no match, return the original state and result \"I don't know what you mean.\""
  [state input-vector]
  (match input-vector
    [:name obj1 obj2 :size size1 size2] (playerPokemons state obj1 obj2 size1 size2 )
                                          
    :else (do 
            (println "I don't know what you mean.")
             state 
              )
    )
)


(defn shuffleverything [state] 
  (let [ pos (rand-stuff-placement 25 #{(get-in state [:pokemons :bulbasur :loc]) 
                                          (get-in state [:pokemons :darkrai :loc]) 
                                          (get-in state [:pokemons :charmainder :loc]) 
                                          (get-in state [:pokemons :squirtle :loc])})] 
   (assoc state :masterPosition pos)
  )
)

(defn declareWinner [state]
   (cond (== (get-in state [:wins]) 2)
         (do
           (println "Congratulations!! You have won the game")
           (update-in state [:masterStatus] :won))
         (== (get-in state [:wins]) -2)
         (do
           (println "Sorry!! You have lost the game")
           (update-in state [:masterStatus] :lose))
         (== (get-in state [:wins]) 1)
         (do
           (println "You have to capture 1 more pokemon to win the game. ")
           state)

         (== (get-in state [:wins]) -1)
         (do
           (println "You have to capture 3 pokemons to win the game. ")
           state)
         :else (do
                 (println "You have to capture 2 pokemons to win the game. ")
                 state
                 )
         )
)

(defn updateWins [state won wildpokemon]
  (if (= won wildpokemon)
  (do 
    (println "Oh no, the wild pokemon " (name wildpokemon) " wins the fight with your pokemon" ) 
    ( declareWinner (update-in (shuffleverything state) [:wins] (fn [x] (- x 1))) ) 
    ) 
  (do
    (println "Congratulations! Your pokemon wins the fight with " (name wildpokemon)
             " . You have captured the wild pokemon.")
    ( declareWinner (update-in (shuffleverything state) [:wins] (fn [x] (+ x 1))) )
    )
   )
     
  )


(defn abs [n] (max n (- n)))

(defn fight [state pokemon wildpokemon]
  (let [win []]
  (if (== (get-in state [:pokemons wildpokemon :level]) (get-in state [:personalPokemonsLevels pokemon :level])  )
    (if (== (rand-int 8) (or 0 1 2 3))
     (updateWins state ((conj win pokemon) 0) wildpokemon )
      (updateWins state ((conj win wildpokemon) 0) wildpokemon )
      )
    (if (== (abs (compare (get-in state [:pokemons wildpokemon :level]) (get-in state [:personalPokemonsLevels pokemon :level])  ))  1)
     (if ( and (== (rand-int 8) (or 0 1 2)) (> (get-in state [:pokemons wildpokemon :level]) (get-in state [:personalPokemonsLevels pokemon :level])  ))
      (updateWins state ((conj win pokemon) 0) wildpokemon )
      (updateWins state ((conj win wildpokemon) 0) wildpokemon )
      )
      (if ( and (== (rand-int 8) (or 0 1 )) (> (get-in state [:pokemons wildpokemon :level]) (get-in state [:personalPokemonsLevels pokemon :level])  ))
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
                                  (name (get-in personalpokemons [:personalPoke1 :name]))
                                  " 2." (name (get-in personalpokemons [:personalPoke2 :name])))
                       (let [pokechoice (read-line)]
                         (if (= pokechoice "1" )
                         (fight state :personalPoke1 :charmainder )
                         (fight state :personalPoke2 :charmainder )
                         )
                      )
                     )    
                     (= selection "R") (shuffleverything state) 
                     )     
                    )
      )   
      (== curloc (get-in state [:pokemons :squirtle :loc]))
      (do (println "You found a wild squirtle of level " 
                   (get-in state [:pokemons :squirtle :level]) 
                   ", do you want to [F]ight or [R]un for the hills? " )
                   (let [selection (read-line)] 
                     (cond (= selection "F") 
                     (do (println "Choose your pokemon to fight: 1." 
                                 (name (get-in personalpokemons [:personalPoke1 :name]))
                                  " 2." (name (get-in personalpokemons [:personalPoke2 :name])))
                       (let [pokechoice (read-line)]
                         (if (= pokechoice "1" )
                         (fight state :personalPoke1 :squirtle )
                         (fight state :personalPoke2 :squirtle )
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
                                  (name (get-in personalpokemons [:personalPoke1 :name]))
                                  " 2." (name (get-in personalpokemons [:personalPoke2 :name])))
                       (let [pokechoice (read-line)]
                         (if (= pokechoice "1" )
                         (fight state :personalPoke1 :bulbasur )
                         (fight state :personalPoke2 :bulbasur )
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
                                  (name (get-in personalpokemons [:personalPoke1 :name]))
                                  " 2." (name (get-in personalpokemons [:personalPoke2 :name])))
                       (let [pokechoice (read-line)]
                         (if (= pokechoice "1" )
                         (fight state :personalPoke1 :darkrai )
                         (fight state :personalPoke2 :darkrai )
                         )
                      )
                     )    
                     (= selection "R") (shuffleverything state) 
                     )     
                     )
      )

      ;;added here
      (== curloc (get-in state [:potion :blue :loc]))
      (if (= (get-in state [:potion :blue :exist]) true) 
          (do
            (println "You found the super rare potion of luck!")
            (println "If you choose to drink it, there is a 50% chance that your pokemons will level up by one! But there is an equal chance that they will level down by one...")
            (println "Do you wish to [D]rink or [I]gnore it?")
            (let [selection (read-line)]
              (cond (= selection "D")
                (do 
                  (let [randChoice (rand-int 2)] ;;0 for minus, 1 for plus
                    (if (= randChoice 1)
                      (do (println "Congratulations! All your pokemons leveled up!")
                          ;; (update-in (update-in myPokeState [:personalPoke1 :level] inc) [:personalPoke2 :level] inc) ;; if increase level
                          (update-in (update-in (update-in state [:personalPokemonsLevels :personalPoke1 :level] (fn [x] (if (= x 2) x (inc x)))) [ :personalPokemonsLevels :personalPoke2 :level] (fn [x] (if (= x 2) x (inc x)))) [:potion :blue :exist] (fn [x] (not x) ))
                      )
                      (do (println "Bad Luck! All your pokemons leveled down...")
                          ;; (update-in (update-in myPokeState [:personalPoke1 :level] dec) [:personalPoke2 :level] dec) ;;if decrease level
                          (update-in (update-in (update-in state [:personalPokemonsLevels :personalPoke1 :level] (fn [x] (if (= x 0) x (dec x)))) [ :personalPokemonsLevels :personalPoke2 :level] (fn [x] (if (= x 0) x (dec x)))) [:potion :blue :exist] (fn [x] (not x) ))
                      )
                    )
                  )
                )
              (= selection "I") state
              )  
            )
          )
        )
      (== curloc (get-in state [:potion :red :loc]))
        (if (= (get-in state [:potion :red :exist]) true)
          (do
            (println "You found the super rare potion of teleportation!")
            (println "If you choose to drink it, you will be teleported near a random pokemon!")
            (println "Do you wish to [D]rink or [I]gnore it?")
            (let [selection (read-line)]
              (cond (= selection "D")
                (do 
                  (let [randChoice (rand-int 4)] ;;0 for charmander, 1 for squirtle, 2 for balbasaur, 3 for darkrai
                    (cond (= randChoice 0);;teleport to fire area
                      (do 
                        (println "You have been teleported!")
                        (if (= (get-in state [:pokemons :charmander :loc]) 1) ;; if pokemon position is at 1
                            (update-in (assoc state :masterPosition 6) [:potion :red :exist] (fn [x] (not x) ) )
                            (update-in (assoc state :masterPosition 1) [:potion :red :exist]  (fn [x] (not x) ) )
                        )
                      )
                      (= randChoice 1);;teleport to water area
                      (do 
                        (println "You have been teleported!")
                        (if (= (get-in state [:pokemons :squirtle :loc]) 8) ;; if pokemon position is at 8
                            (update-in (assoc state :masterPosition 9) [:potion :red :exist] (fn [x] (not x) ) )
                            (update-in (assoc state :masterPosition 8) [:potion :red :exist] (fn [x] (not x) ) )
                        )
                      )
                      (= randChoice 2);;teleport to nature area
                      (do 
                        (println "You have been teleported!")
                        (if (= (get-in state [:pokemons :bulbasur :loc]) 23) ;; if pokemon position is at 23
                            (update-in (assoc state :masterPosition 22) [:potion :red :exist] (fn [x] (not x) ))
                            (update-in (assoc state :masterPosition 23) [:potion :red :exist] (fn [x] (not x) ))
                        )
                      )
                      (= randChoice 3);;teleport to dark area
                      (do 
                        (println "You have been teleported!")
                        (if (= (get-in state [:pokemons :darkrai :loc]) 16) ;; if pokemon position is at 16
                            (update-in (assoc state :masterPosition 15) [:potion :red :exist] (fn [x] (not x) ))
                            (update-in (assoc state :masterPosition 16) [:potion :red :exist] (fn [x] (not x) ))
                        )
                      )
                    )
                  )
                )
              (= selection "I") state
              )
            )
          )
      )
      :else state 
  )
 )
)



(defn printeverything [state]
  (println "The player inventory is: ")
  (println "Your first personal pokemon is" (name (get-in myPokeState [:personalPoke1 :name])) 
           ". It's size is" (name (get-in myPokeState [:personalPoke1 :size])) 
           ". It's level is" (get-in state [:personalPokemonsLevels :personalPoke1 :level])  ".")
  (println "Your second personal pokemon is" (name (get-in myPokeState [:personalPoke2 :name]))
           ". It's size is" (name (get-in myPokeState [:personalPoke2 :size]))
           ". It's level is" (get-in state [:personalPokemonsLevels :personalPoke2 :level]) ".")
  )
        

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
          (if (= (react state (vec input)) state) 
            (recur state) 
            ())
    )
   )
  

  (loop [state env]
    ;; (println state)
    (if (= (:masterStatus state) :alive)
      (do
    
        (println "What do you want to do? ([S]earch/[Q]uit/[I]nventory)/[M]ap)" ) 
        (let [selection (read-line)] 
          (cond (= selection "S") 
                (do  
                  (lookingAround state)
                  (println "You can move in the given directions: " (direction (-> :masterPosition state))
                  " What do you want to do? (For Right - R / Left - L / Up - U / Down - D)")
                  (let [dir (read-line)]
                          (cond (some #{ (numerical-direction state dir) } (-> :masterPosition state pokespace))
                                (recur (evaluateAction (assoc state :masterPosition (numerical-direction state dir) ) myPokeState )) 
                                :else (do 
                                        (println "You can't go there.")
                                        (recur state)
                                        )
                            )
                    )
                  )
                (= selection "Q") (println "Thanks for playing Capture the Pokemons!")
                (= selection "I") (do 
                                    (printeverything state)
                                    (recur state)
                                    )
                ;;added here                    
                (= selection "M") (do 
                  (let [currPos (get-in state [:masterPosition])]
                 
                    (cond (some #(= currPos %) (get-in state [:fire_area ]))
                            (println "You are currently in the fire area, You may run into a Charmainder...")
                          (some #(= currPos %) (get-in state [:water_area ]))
                            (println "You are currently in the water area, You may run into a Squirtle...")
                          (some #(= currPos %) (get-in state [:nature_area]))
                            (println "You are currently in the nature area, You may run into a Bulbasaur...")
                          (some #(= currPos %) (get-in state [:dark_area]))
                            (println "You are currently in the water area, You may run into a Darkrai...")
                          :else (println "You are currently in the starting position")
                    )
                  )
                  (recur state)
                )
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
  

(defn -main
  "Start the REPL with the initial environment."
  []
  (repl (initial-env)
  )
)