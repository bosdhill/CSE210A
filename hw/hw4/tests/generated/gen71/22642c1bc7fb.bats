load ../../harness

@test "22642c1bc7fb" {
  check 'L:=  -1   *x    ;  z  :=    -2 + 0' '⇒ skip; z := (-2+0), {L → 0}
⇒ z := (-2+0), {L → 0}
⇒ skip, {L → 0, z → -2}'
}
