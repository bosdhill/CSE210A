load ../../harness

@test "fe186c9228d8" {
  check 'while x    +  x <     2*     -1  ∧true    do x  :=     P7     -Kb   ' '⇒ skip, {}'
}
