load ../../harness

@test "d0a7253bcf8b" {
  check 'qw  :=     y*   4 ;x :=     -2' '⇒ skip; x := -2, {qw → 0}
⇒ x := -2, {qw → 0}
⇒ skip, {qw → 0, x → -2}'
}
