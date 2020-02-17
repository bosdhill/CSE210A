load ../../harness

@test "d925ae0919d0" {
  check 'y := 1    *     1    ; 
skip   ' '⇒ skip; skip, {y → 1}
⇒ skip, {y → 1}'
}
