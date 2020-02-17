load ../../harness

@test "320978d00c90" {
  check 'x   :=    3  +  -1    ' '⇒ skip, {x → 2}'
}
