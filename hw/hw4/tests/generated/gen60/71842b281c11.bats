load ../../harness

@test "71842b281c11" {
  check 'while y    + zn   =    2 + -2 ∨   0   +   k     =  z   +   -2 do  



y   := x    + -1 ' '⇒ y := (x+-1); while (((y+zn)=(2+-2))∨((0+k)=(z+-2))) do { y := (x+-1) }, {}
⇒ skip; while (((y+zn)=(2+-2))∨((0+k)=(z+-2))) do { y := (x+-1) }, {y → -1}
⇒ while (((y+zn)=(2+-2))∨((0+k)=(z+-2))) do { y := (x+-1) }, {y → -1}
⇒ skip, {y → -1}'
}
