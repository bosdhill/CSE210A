load ../../harness

@test "988be13583d1" {
  check 'while (¬true)      do    y    :=     4*  -4' '⇒ skip, {}'
}
