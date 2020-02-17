load ../../harness

@test "0ff79afa596c" {
  check 'while false∧     4     =y     +x      do   x  :=   y     -  -3  ' '⇒ skip, {}'
}
