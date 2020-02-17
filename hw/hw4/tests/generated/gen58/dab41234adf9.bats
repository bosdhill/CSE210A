load ../../harness

@test "dab41234adf9" {
  check 'if (y    = 3    *   4  ∧   true)      then skip    else Mm  := -2     - z     ' '⇒ Mm := (-2-z), {}
⇒ skip, {Mm → -2}'
}
