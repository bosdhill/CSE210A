load ../../harness

@test "8e9e419be74f" {
  check 'while -2+     z   <y    +     x  ∧ 1   *     -4     <  x *    Jg      do 
y     :=  x     +-3     ' '⇒ y := (x+-3); while (((-2+z)<(y+x))∧((1*-4)<(x*Jg))) do { y := (x+-3) }, {}
⇒ skip; while (((-2+z)<(y+x))∧((1*-4)<(x*Jg))) do { y := (x+-3) }, {y → -3}
⇒ while (((-2+z)<(y+x))∧((1*-4)<(x*Jg))) do { y := (x+-3) }, {y → -3}
⇒ skip, {y → -3}'
}
