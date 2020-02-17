load ../../harness

@test "d9558215c067" {
  check 'while 4     * C=     x -    y   do 
  x   :=   0   -    4 ' '⇒ x := (0-4); while ((4*C)=(x-y)) do { x := (0-4) }, {}
⇒ skip; while ((4*C)=(x-y)) do { x := (0-4) }, {x → -4}
⇒ while ((4*C)=(x-y)) do { x := (0-4) }, {x → -4}
⇒ skip, {x → -4}'
}
