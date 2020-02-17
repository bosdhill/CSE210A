load ../../harness

@test "a3ae927e1db8" {
  check 'while -3    *xa=  y     + 3∨  4     *    z = z -  y do 

y :=   -3*   2 ' '⇒ y := (-3*2); while (((-3*xa)=(y+3))∨((4*z)=(z-y))) do { y := (-3*2) }, {}
⇒ skip; while (((-3*xa)=(y+3))∨((4*z)=(z-y))) do { y := (-3*2) }, {y → -6}
⇒ while (((-3*xa)=(y+3))∨((4*z)=(z-y))) do { y := (-3*2) }, {y → -6}
⇒ skip, {y → -6}'
}
