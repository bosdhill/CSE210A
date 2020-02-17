load ../../harness

@test "e2d8b7dd97ee" {
  check 'while (¬true)     do  y :=     -1 *-1     ' '⇒ skip, {}'
}
