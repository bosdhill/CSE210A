load ../../harness

@test "b348cb641440" {
  check 'while y    <0     - z      do y :=    2 ' '⇒ skip, {}'
}
