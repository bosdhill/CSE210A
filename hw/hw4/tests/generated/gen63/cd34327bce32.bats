load ../../harness

@test "cd34327bce32" {
  check 'while false∧   false  do x   :=1     -     3     ' '⇒ skip, {}'
}
