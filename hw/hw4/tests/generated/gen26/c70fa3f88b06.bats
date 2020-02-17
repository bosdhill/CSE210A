load ../../harness

@test "c70fa3f88b06" {
  check 'while (¬(-3-   y  =    z)) do   y     :=    -3 +  z   ' '⇒ y := (-3+z); while ¬((-3-y)=z) do { y := (-3+z) }, {}
⇒ skip; while ¬((-3-y)=z) do { y := (-3+z) }, {y → -3}
⇒ while ¬((-3-y)=z) do { y := (-3+z) }, {y → -3}
⇒ skip, {y → -3}'
}
