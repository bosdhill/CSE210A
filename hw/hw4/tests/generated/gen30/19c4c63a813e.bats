load ../../harness

@test "19c4c63a813e" {
  check 'if (¬(E     - z     <4    *   z)) then x := 1    *  0 else    skip  ' '⇒ x := (1*0), {}
⇒ skip, {x → 0}'
}
