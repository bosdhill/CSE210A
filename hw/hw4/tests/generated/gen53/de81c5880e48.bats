load ../../harness

@test "de81c5880e48" {
  check 'while -1  < -3    *    3    ∨     -1   <   2+ y     do  

y  :=   -3     - m    ' '⇒ y := (-3-m); while ((-1<(-3*3))∨(-1<(2+y))) do { y := (-3-m) }, {}
⇒ skip; while ((-1<(-3*3))∨(-1<(2+y))) do { y := (-3-m) }, {y → -3}
⇒ while ((-1<(-3*3))∨(-1<(2+y))) do { y := (-3-m) }, {y → -3}
⇒ skip, {y → -3}'
}
