load ../../harness

@test "3afd6acfb534" {
  check 'b :=     y -  x  ;   g2:=     1    +    z ' '⇒ skip; g2 := (1+z), {b → 0}
⇒ g2 := (1+z), {b → 0}
⇒ skip, {b → 0, g2 → 1}'
}
