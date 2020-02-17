load ../../harness

@test "0535fc7d18e9" {
  check 'gN  := z    + y   ;D     :=     z*  x    ' '⇒ skip; D := (z*x), {gN → 0}
⇒ D := (z*x), {gN → 0}
⇒ skip, {D → 0, gN → 0}'
}
