load ../../harness

@test "77305670aa4e" {
  check 'y    :=   -4  ; 


z    := 4     + -3' '⇒ skip; z := (4+-3), {y → -4}
⇒ z := (4+-3), {y → -4}
⇒ skip, {y → -4, z → 1}'
}
