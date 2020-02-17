load ../../harness

@test "e65d5acc4b7c" {
  check 'while false    ∨ -1-    2    <     XU     +  y  do  
 y    :=  -3 +     Tn   ' '⇒ y := (-3+Tn); while (false∨((-1-2)<(XU+y))) do { y := (-3+Tn) }, {}
⇒ skip; while (false∨((-1-2)<(XU+y))) do { y := (-3+Tn) }, {y → -3}
⇒ while (false∨((-1-2)<(XU+y))) do { y := (-3+Tn) }, {y → -3}
⇒ skip, {y → -3}'
}
