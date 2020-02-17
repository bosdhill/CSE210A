load ../../harness

@test "30e9cdcb400c" {
  check 'while false   do skip' '⇒ skip, {}'
}
