load ../../harness

@test "129c3f1ce8f2" {
  check 'while z = y   +  0    ∧ true do  
 y:=   y +    -3   ' '⇒ y := (y+-3); while ((z=(y+0))∧true) do { y := (y+-3) }, {}
⇒ skip; while ((z=(y+0))∧true) do { y := (y+-3) }, {y → -3}
⇒ while ((z=(y+0))∧true) do { y := (y+-3) }, {y → -3}
⇒ skip, {y → -3}'
}
