load ../../harness

@test "a4dd0443e931" {
  check 'while true    ∧   false  do y   :=    -3     +     -2 ' '⇒ skip, {}'
}
