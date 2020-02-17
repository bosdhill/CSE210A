load ../../harness

@test "d0e0a293842e" {
  check 'y  :=    y     +  z   ;z := z   +  y ' '⇒ skip; z := (z+y), {y → 0}
⇒ z := (z+y), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
