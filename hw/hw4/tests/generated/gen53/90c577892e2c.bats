load ../../harness

@test "90c577892e2c" {
  check 'if (true    ∧    false)  then 
skip  else x  :=-2    *   x  ' '⇒ x := (-2*x), {}
⇒ skip, {x → 0}'
}
