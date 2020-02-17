load ../../harness

@test "36817ba66bbf" {
  check 'x :=     4     -     1 ' '⇒ skip, {x → 3}'
}
