load ../../harness

@test "e082ed8793c0" {
  check 'while false     ∨ x  -  0     <x *   -1   do skip    ' '⇒ skip, {}'
}
