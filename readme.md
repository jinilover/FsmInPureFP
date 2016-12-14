#Finite State Machine in pure functional programming
My first application in Haskell for learning Haskell.
Haskell is pure FP language which implies no side effect.  Although FSM contains the name state, it can be implemented in pure FP such as Haskell.

An FSM can be implemented in pure FP easily according to this function
```Haskell
transit :: State -> Event -> State
```
In FP applications, the key is the algebraic design (or called Type Driven Development) specific to the business domain.  The electronic pet is an FSM - when the current **State** is applied the user input including no input (**Event**), it will transit to another **State**.

##Core algebraic data types
To make the description simple, detailed programing constructs are not mentioned here.  Please refer to the source for more details.

The **State** types refer to the stages the pet has to undergo and the properties possessed by each stage.  To make the life simpler, I only define 4 stages as follows.
```Haskell
data Stage = Egg | Chicken | Adult | Elder
```

The **Event** types are user input.
```Haskell
data Input = IncreaseTemp | DecreaseTemp | Feed | Play | Medication | Bed | Sing
```
Not all `Input` types are applicable to each `Stage`.  E.g. `IncreaseTemp` and `DecreaseTemp` are used to changing the egg's temperature during hatching, they are applicable to `Egg` only.  A function `inputsByState` is used to provide the appropriate inputs for the user according to the current `Stage` and the properties possessed by it.

##Auxiliary algebraic data types
They represent the properties possessed by each `Stage`.
```Haskell
data Health = Dead | Sick | Weak | Fair | Healthy | Hardy

data Fullness = Full { fullnessTime :: UTCTime } |
                SoSo { fullnessTime :: UTCTime } |
                Hungry { fullnessTime :: UTCTime }

data Status = Sleeping { statusTime :: UTCTime } | Awake { statusTime :: UTCTime }

data PooAmount = PooAmount { pooTime :: UTCTime, poo :: Int }
data Mood = Mood { moodTime :: UTCTime, moodValue :: Int}
data Fatigue = Fatigue { fatigueTime :: UTCTime, fatigueValue :: Int }
```

For the common data types possesed by different `Stage` and the functions using them, please refer to 
https://github.com/jinilover/FsmInPureFP/blob/master/src/Fsm/Commons.hs
https://github.com/jinilover/FsmInPureFP/blob/master/src/Fsm/UpdateStates.hs

For the types modelling the `Stage` and the functions using them, please refer to 
https://github.com/jinilover/FsmInPureFP/blob/master/src/Fsm/Stages.hs

#Rule of the game
The basic idea is similar to the commercial game - goes through several stages in the lifecycle.  User needs to enter appropriate input to keep it alive and growing.  If the user doesn't enter input for certain period of time `inputTimeout` or enter an invalid input, it will proceed to another state accordingly.  When it's sleeping, it doesn't want to be disturbed and user is not allowed to enter input.  To make the game simple, there are only 4 stages - `Egg`, `Chicken`, `Adult` and `Elder`.  Since each stage has different behaviour and properties, the available inputs are not the same in all stages.  

##Egg
Egg should be kept for a certain temperature to be hatched.  Temperature can be raised to speed up the hatching.  But there is a max. allowed temperature `fatalMaxTemp`.  Exceeding this limit or leave it for certain period of time `fatalTempSecs` will cook the egg.  Similarly there is a min. allowed temperature `fatalMinTemp`.

Available inputs: 
* `IncreaseTemp`, 
* `DecreaseTemp`

##Chicken
Its behaviour:
* Getting hungry or digesting the feeding to produce poo, gain weight/length or health weakened from hungry after certain period of time `digestFullnessSecs`.
* Health weakened after constipated for certain period of time `pooLimitSecs`.
* Mood downgraded after being ignored for certain period of time `decreaseMoodSecs`.
* Health weakened after depressed for certain period of time `depressSecs`.
* Getting more tired after being awake for a long time.
* Health weakened after being fatigue for certain period of time `fatigueSecs`.
* Will poop when its poo amount reach `pooLimit`.  However, sometimes it does not poop itself even the amount reaches `pooLimit`.  This aims to show it is capable to downgrade the health after being constipated for `pooLimitSec`.
* Go to sleep when its fatigue reach `fatigueLimit`.  However, sometimes it does not sleep itself even the fatigue reaches `fatigueLimit`.  This aims to show it is capable to downgrade the health after being fatigue for `fatigueSecs`.
* Transit to `Adult` when it reaches the specified `age`.

Available inputs:
* `Feed` - change its fullness which subsequently affect how it behaves after certain period of time `digestFullnessSecs`.
* `Play` - depends on its current state, this input may make it digest to gain or loss weight, become taller, produce more poo but also happier and healthier.
* `Medication` - this input is available only when it's sick.
* `Bed` - it will refuse to sleep if it has been awakened for < `maxAwakeSecs`.  The difference from it sleeps itself is: when you put it to bed, it will sleep only it has been awaken for `maxAwakeSecs` without caring its fatigue level.  On the other hand, it will sleep itself only only if its fatigue reaches `fatigueLimit` without caring if it has been awaken for `maxAwakeSecs`.

##Adult
Similar to `Chicken` except the parameters are different.  It has 1 more option `Sing` to make it happy.

##Elder
Similar to `Chicken` except the parameters are different.
Additional behaviour:
* Health is weakened automatically after certain period of time `weakerInSecs`.
* Limited # of medication.

#How to run it?
To build it, run `stack build` and run `stack exec fsm-purefp-exe` to execute it.

#How to run the unit-test?
Under `FsmInPureFP` folder, run command
`stack build FsmInPureFP:FsmInPureFP-tests`

#Parameters to be tuned
https://github.com/jinilover/FsmInPureFP/blob/master/src/resources/appl.cfg
