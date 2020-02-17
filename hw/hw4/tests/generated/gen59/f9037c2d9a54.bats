load ../../harness

@test "f9037c2d9a54" {
  check 'x    :=  x   * Kc    ;  
y     :=  -1   *  0   ' '⇒ skip; y := (-1*0), {x → 0}
⇒ y := (-1*0), {x → 0}
⇒ skip, {x → 0, y → 0}'
}
