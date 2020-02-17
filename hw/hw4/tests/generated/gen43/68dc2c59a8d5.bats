load ../../harness

@test "68dc2c59a8d5" {
  check 'y    :=  z -   -1   ;
 y :=y-     z    ' '⇒ skip; y := (y-z), {y → 1}
⇒ y := (y-z), {y → 1}
⇒ skip, {y → 1}'
}
