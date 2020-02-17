load ../../harness

@test "5d23af4d4c88" {
  check 'if (true  ∧ y     *    z  <  -4    *    -1) then x    := 4    --4    else   skip     ' '⇒ x := (4--4), {}
⇒ skip, {x → 8}'
}
