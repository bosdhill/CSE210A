load ../../harness

@test "0665b32049c0" {
  check 'while y    -    B  =   x     -   y   ∨ false  do 



x  :=   1   +    -2  ' '⇒ x := (1+-2); while (((y-B)=(x-y))∨false) do { x := (1+-2) }, {}
⇒ skip; while (((y-B)=(x-y))∨false) do { x := (1+-2) }, {x → -1}
⇒ while (((y-B)=(x-y))∨false) do { x := (1+-2) }, {x → -1}
⇒ skip, {x → -1}'
}
