load ../../harness

@test "ece0dd071fc7" {
  check 'while (¬(-1   < z))   do  y     :=    y ' '⇒ skip, {}'
}
