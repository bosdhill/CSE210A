load ../../harness

@test "6f846bfb6156" {
  check 'skip    ; z     :=   3   -  1  ' '⇒ z := (3-1), {}
⇒ skip, {z → 2}'
}
