load ../../harness

@test "f38afc8a675b" {
  check 'skip     ;   z :=2   +  4   ' '⇒ z := (2+4), {}
⇒ skip, {z → 6}'
}
