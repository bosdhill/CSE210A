load ../../harness

@test "f999efb23d32" {
  check 'skip     ;z  :=  x+ y     ' '⇒ z := (x+y), {}
⇒ skip, {z → 0}'
}
