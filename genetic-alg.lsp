;;;; Problem: Traveling Salesman Genetic Algorithm
;;;; File: tsm.lsp

;; Description

; It's Christmas Eve, and Santa has a busy night. He needs to find the fastest route between all
; his destinations this evening. Santa needs to start at Santas-Workshop and end the night at 
; Rudolfs-Runaway, the local dive. Anything in-between is fair game.

; City distance data are taken from https://people.sc.fsu.edu/~jburkardt/datasets/tsp/p01_d.txt,
; to ensure that inter-city distances are spatially realistic. City names are assigned to these 
; points arbitrarily. Distances are assigned arbitrarily (but consistently).

;; Imports

( load "../challenge3/lp.lsp" )

;; Constants and Other Methods

( setf *cities-pl* '( (SANTAS-WORKSHOP . SW) (ELFDALE . ED) (CANDY-CANE-FOREST . CF) (GUM-DROP-VALLEY . GV) (POLAR-PALACE . PP)
				   (FROZEN-FIELDS . FF) (HEALTHY-HAMLET . HH) (EVERGREEN-CITY . EC) (THE-SNOW-BANK . SB) (MRS-CLAUSES-PARENTS . CP)
				   (JINGLE-BELL-JAIL . JJ) (MISTLETOE-MOUNTAIN . MM) (TINSELTOWN . TT) (GIFT-GULF . GG) (RUDOLFS-RUNAWAY . RR) ) )

( setf *cities* ( mapcar #'cdr *cities-pl* ) )

( defmethod pop-f-l ( ( lst list ) )
  ( setf lst ( cdr lst ) )
  ( setf lst ( rdc lst ) )
)

( setf *inner-cities* ( pop-f-l *cities* ) )
				   
( setf ( symbol-plist 'SW ) '( ED 29 CF 82 GV 46 PP 68 FF 52 HH 72 EC 42 SB 51 CP 55 JJ 29 MM 74 TT 23 GG 72 RR 46 ) )
( setf ( symbol-plist 'ED ) '( SW 29 CF 55 GV 46 PP 42 FF 43 HH 43 EC 23 SB 23 CP 31 JJ 41 MM 41 TT 11 GG 52 RR 21 ) )
( setf ( symbol-plist 'CF ) '( SW 82 ED 55 GV 68 PP 46 FF 55 HH 23 EC 43 SB 41 CP 21 JJ 79 MM 21 TT 64 GG 31 RR 51 ) )
( setf ( symbol-plist 'GV ) '( SW 46 ED 46 CF 68 PP 82 FF 15 HH 72 EC 31 SB 62 CP 42 JJ 21 MM 51 TT 51 GG 43 RR 64 ) )
( setf ( symbol-plist 'PP ) '( SW 68 ED 42 CF 46 GV 82 FF 74 HH 23 EC 52 SB 21 CP 46 JJ 82 MM 58 TT 46 GG 65 RR 23 ) )
( setf ( symbol-plist 'FF ) '( SW 52 ED 43 CF 55 GV 15 PP 74 HH 61 EC 23 SB 55 CP 31 JJ 33 MM 37 TT 51 GG 29 RR 59 ) )
( setf ( symbol-plist 'HH ) '( SW 72 ED 43 CF 23 GV 72 PP 23 FF 61 EC 42 SB 23 CP 31 JJ 77 MM 37 TT 51 GG 46 RR 33 ) )
( setf ( symbol-plist 'EC ) '( SW 42 ED 23 CF 43 GV 31 PP 52 FF 23 HH 42 SB 33 CP 15 JJ 37 MM 33 TT 33 GG 31 RR 37 ) )
( setf ( symbol-plist 'SB ) '( SW 51 ED 23 CF 41 GV 61 PP 21 FF 55 HH 23 EC 33 CP 29 JJ 62 MM 46 TT 29 GG 51 RR 11 ) )
( setf ( symbol-plist 'CP ) '( SW 55 ED 31 CF 21 GV 42 PP 46 FF 31 HH 31 EC 15 SB 29 JJ 51 MM 21 TT 41 GG 23 RR 37 ) )
( setf ( symbol-plist 'JJ ) '( SW 29 ED 41 CF 71 GV 21 PP 82 FF 33 HH 77 EC 37 SB 62 CP 51 MM 65 TT 42 GG 59 RR 61 ) )
( setf ( symbol-plist 'MM ) '( SW 74 ED 41 CF 21 GV 51 PP 58 FF 37 HH 37 EC 33 SB 46 CP 21 JJ 65 TT 61 GG 11 RR 55 ) )
( setf ( symbol-plist 'TT ) '( SW 23 ED 11 CF 64 GV 51 PP 46 FF 51 HH 51 EC 33 SB 29 CP 41 JJ 42 MM 61 GG 62 RR 23 ) )
( setf ( symbol-plist 'GG ) '( SW 72 ED 52 CF 31 GV 43 PP 65 FF 29 HH 46 EC 31 SB 51 CP 23 JJ 59 MM 11 TT 62 RR 59 ) )
( setf ( symbol-plist 'RR ) '( SW 46 ED 21 CF 51 GV 64 PP 23 FF 59 HH 33 EC 37 SB 11 CP 37 JJ 61 MM 55 TT 23 GG 59 ) )

( defmethod push-SW-RR ( ( lst list ) )
  ( append ( list ( car *cities* ) ) lst ( list ( rac *cities* ) ) )
)

( defmethod distance ( ( lst list ) )
  ( if ( singleton-p lst )
    0
	( + ( get ( car lst ) ( cadr lst ) ) ( distance ( cdr lst ) ) )
  )
)

;; Task 1: Random Tour

( defmethod random-tour ()
  ( push-SW-RR ( random-permutation *inner-cities* ) )
)

;; Task 2: Mutation

( defmethod random-positions ( &aux p q ) 
  ( setf p ( random ( length *inner-cities* ) ) )
  ( setf q ( random ( length *inner-cities* ) ) )
  
  ( if ( = p q )
    ( random-positions )
    ( cons p q )
  )
)

( defmethod mutation ( ( lst list ) &aux temp rp )
  ( setf lst ( pop-f-l lst ) )
  ( setf rp ( random-positions ) )
  
  ( rotatef ( nth ( car rp ) lst ) ( nth ( cdr rp ) lst ) )
  ( setf lst ( push-SW-RR lst ) )
)

;; Task 3: Crossover

( defmethod clone ( ( l list ) )
  ( if ( null l )
    NIL
    ( cons ( car l ) ( clone ( cdr l ) ) )
  )
)

(defmethod crossover ( ( m list ) ( f list ) &aux pos child temp visited )
  ( setf m ( pop-f-l m ) )
  ( setf f ( pop-f-l f ) )
  ( setf pos ( random ( length m ) ) )
  
  ( setf child ( append (first-n m pos ) ( rest-n f pos ) ) )

  ( setf temp ( clone *inner-cities* ) )
  ( dolist ( i child )
    ( setf temp ( remove i temp ) )
  )
  ( setf visited NIL )
  ( setf child ( mapcar ( lambda ( x ) 
    ( if ( member x visited )
      ( progn
        ( setf x ( pick temp ) )
        ( setf temp ( remove x temp ) )
        x
      )
      ( progn
        ( setf visited ( cons x visited ) )
        x
      )
    )
  ) child ) )
	
  ( push-SW-RR child )
)

( defmethod first-n ( ( lst list ) ( n number ) )
  ( if ( = n 0 )
    NIL
	( cons ( car lst ) ( first-n ( cdr lst ) ( - n 1 ) ) )
  )
)

( defmethod rest-n ( ( lst list ) ( n number ) )
  ( if ( = n 0 )
    lst
    ( rest-n ( cdr lst ) ( - n 1 ) )
  )
)

;; Task 4: Demo Programs for Mutation and Crossover

( defmethod mutation-demo ( &aux s m )
  ( setf s ( random-tour ) )
  ( dotimes ( i 10 )
    ( format t "s = ~A~%" s )
	( setf m ( mutation s ) )
	( format t "m = ~A~%~%" m )
  )
)

( defmethod crossover-demo ( &aux m f x )
  ( setf m ( random-tour ) )
  ( setf f ( random-tour ) )
  ( dotimes ( i 10 )
    ( format t "m = ~A~%" m )
	( setf x ( crossover m f ) )
	( format t "x = ~A~%" x )
	( format t "f = ~A~%~%" f )
  )
)

;; Task 5: The Fitness Metric

( defmethod fitness ( ( lst list ) )
  ( distance lst )
)

;; Task 6: The Individual Class

( defclass individual ()
  (
    ( tour :accessor individual-tour :initarg :tour )
	( fitness :accessor individual-fitness :initarg :fitness )
	( number :accessor individual-number :initarg :number )
  )
)

( defmethod random-individual ( &aux tour )
  ( setf tour ( random-tour ) )
  ( make-instance 'individual
    :tour tour
	:fitness ( fitness tour )
	:number 0
  )
)

( defmethod new-individual ( ( nr number ) ( tour list ) )
  ( make-instance 'individual
    :tour tour
	:fitness ( fitness tour )
	:number nr
  )
)


( defmethod display ( ( i individual ) )
  ( format t "~4A ~A [~A] ~%" ( individual-number i ) ( individual-tour i ) ( individual-fitness i ) )
)

;; Task 7: The Population Class

( setf *population-size* 100 )
( setf *selection-size* 10 )

( defclass population ()
  (
    ( individuals :accessor population-individuals :initarg :individuals )
	( generation :accessor population-generation :initarg :generation :initform 0 )
  )
)

( defmethod size ( ( p population ) )
  ( length ( population-individuals p ) )
)

( defmethod initial-population ( &aux individuals )
  ( setf individuals () )
  ( dotimes ( i *population-size* )
    ( push ( new-individual ( + i 1 ) ( random-tour ) ) individuals )
  )
  ( make-instance 'population :individuals ( reverse individuals ) )
)

( defmethod average ( ( p population ) )
  ( /
    ( reduce #'+ ( mapcar #'individual-fitness ( population-individuals p ) ) ) 
	( size p )
  )
)

( defmethod select-individual ( ( p population ) &aux candidates mfi )
  ( setf candidates ( select-individuals p ) )
  ( setf mfi ( most-fit-individual candidates ) )
  mfi
)

( defmethod select-individuals ( ( p population ) &aux individuals candidates rn )
  ( setf individuals ( population-individuals p ) )
  ( setf candidates () )
  ( dotimes ( i *selection-size* )
    ( setf rn ( random *population-size* ) )
	( push ( nth rn individuals ) candidates )
  )
  candidates
)

( defmethod most-fit-individual ( ( lst list ) &aux max-value max-individual )
  ( reduce 
    ( lambda ( a b )
      ( if ( < ( individual-fitness a ) ( individual-fitness b ) ) a b )
    )
    lst
  )
)

( defmethod display  ( ( p population ) )
  ( format t "Population ~2A: ~%" ( population-generation p ) )
  ( dolist ( i ( population-individuals p ) )
    ( display i )
  )
)

;; Task 8: Incorporating Mutation

( setf *mutation-chance* 50 )

( defmethod mutate ( ( i individual ) &aux mutation )
  ( setf mutation ( mutation ( individual-tour i ) ) )
  ( make-instance 'individual
    :number ( individual-number i )
	:tour mutation
	:fitness ( fitness mutation )
  )
)

( defmethod maybe-mutate ( ( i individual ) )
  ( if ( <= ( + 1 ( random 100 ) ) *mutation-chance* )
    ( mutate i )
	i
  )
)

;; Task 9: Copy!

( setf *copy-chance* 40 )

( defmethod perform-copies ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-copies ) )
    ( perform-one-copy cp np )
  )
)

( defmethod nr-copies ()
  ( * ( / *copy-chance* 100 ) *population-size* )
)

( defmethod perform-one-copy ( ( cp population ) ( np population ) &aux m mm new-i )
  ( setf m ( select-individual cp ) )
  ( setf mm ( maybe-mutate m ) )
  ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
  ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-tour mm ) ) )
  ( setf
    ( population-individuals np )
	( append ( population-individuals np ) ( list new-i ) )
  )
  NIL
)

;; Task 10: Crossover!

( setf *crossover-chance* 60 )

( defmethod perform-crossovers ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-crossovers ) )
    ( perform-one-crossover cp np )
  )
)

( defmethod nr-crossovers ()
  ( * ( / *crossover-chance* 100 ) *population-size* )
)

( defmethod perform-one-crossover ( ( cp population ) ( np population ) &aux m mm mother father new-i )
  ( setf mother ( select-individual cp ) )
  ( setf father ( select-individual cp ) )
  ( setf m ( crossover mother father ) )
  ( setf mm ( maybe-mutate m ) )
  ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
  ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-tour mm ) ) )
  ( setf ( population-individuals np ) ( append ( population-individuals np ) ( list new-i ) ) )
  NIL
)

( defmethod crossover ( ( mother individual ) ( father individual ) )
  ( new-individual 0 ( crossover ( individual-tour mother ) ( individual-tour father ) ) )
)

;; Task 11:

( setf *number-generations* 25 )

( defmethod next-generation ( ( cp population ) &aux np )
  ( setf np ( make-instance 'population 
                :generation ( + 1 ( population-generation cp ) )
                :individuals NIL )
  )
  ( perform-copies cp np )
  ( perform-crossovers cp np )
  np
)

( defmethod ga-text-demo ( &aux p )
  ( setf p ( initial-population ) )
  ( summarize p )
  ( terpri )
  ( dotimes ( i *number-generations* )
    ( setf p ( next-generation p ) )
    ( format t "Average fitness of generation ~2A = ~A ~%" 
      ( population-generation p ) ( float ( average p ) )
    )
  )
  ( terpri )
  ( summarize p )
  ( format t "Most fit individual of the final population: ~A [~A] ~% ~A ~%" 
    ( individual-tour ( most-fit-individual ( population-individuals p ) ) )
    ( individual-fitness ( most-fit-individual ( population-individuals p ) ) )
	( mapcar ( lambda ( x ) ( car ( rassoc x *cities-pl* ) ) ) ( individual-tour ( most-fit-individual ( population-individuals p ) ) ) )
  )
  ( terpri )
  NIL
)

( defmethod summarize ( ( p population ) )
  ( display p )
  ( format t "Average fitness of generation ~2A = ~A ~%" 
    ( population-generation p ) ( float ( average p ) )
  )
  ( terpri )
)
