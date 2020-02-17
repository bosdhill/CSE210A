load ../../harness

@test "63cfa3388d82" {
  check 'if (y   *     y  =    -4 - -1 ∧ z-  z     <   4    -  3)   then skip else x :=    1  + 2    ' '⇒ x := (1+2), {}
⇒ skip, {x → 3}'
}
