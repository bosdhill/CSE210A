load ../../harness

@test "77c1786d36e3" {
  check 'while (¬true)  do G2   :=   x   +3' '⇒ skip, {}'
}
