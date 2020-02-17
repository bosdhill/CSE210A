load ../../harness

@test "c7faa429d3e2" {
  check 'if (false   ∧    true)    then skip   else 
z  := y     -     4    ' '⇒ z := (y-4), {}
⇒ skip, {z → -4}'
}
