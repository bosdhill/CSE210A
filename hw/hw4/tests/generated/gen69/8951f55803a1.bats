load ../../harness

@test "8951f55803a1" {
  check 'L   :=1 -    X;
x     :=    -1     - y  ' '⇒ skip; x := (-1-y), {L → 1}
⇒ x := (-1-y), {L → 1}
⇒ skip, {L → 1, x → -1}'
}
