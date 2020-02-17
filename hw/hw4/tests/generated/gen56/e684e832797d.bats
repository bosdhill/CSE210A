load ../../harness

@test "e684e832797d" {
  check 'if (true    ∧  true)  then 



x    := y    *    4   else   x   :=    -3   - 2  ' '⇒ x := (y*4), {}
⇒ skip, {x → 0}'
}
