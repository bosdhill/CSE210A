load ../../harness

@test "560087320ecb" {
  check 'while x    =   z   -   x do   x    :=   -4    +     -2   ' '⇒ x := (-4+-2); while (x=(z-x)) do { x := (-4+-2) }, {}
⇒ skip; while (x=(z-x)) do { x := (-4+-2) }, {x → -6}
⇒ while (x=(z-x)) do { x := (-4+-2) }, {x → -6}
⇒ skip, {x → -6}'
}
