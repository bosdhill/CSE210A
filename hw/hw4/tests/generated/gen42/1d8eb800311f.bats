load ../../harness

@test "1d8eb800311f" {
  check 'while (¬(s     *   z   <     x   +   z)) do  


 x    :=    2   +    z   ' '⇒ x := (2+z); while ¬((s*z)<(x+z)) do { x := (2+z) }, {}
⇒ skip; while ¬((s*z)<(x+z)) do { x := (2+z) }, {x → 2}
⇒ while ¬((s*z)<(x+z)) do { x := (2+z) }, {x → 2}
⇒ skip, {x → 2}'
}
