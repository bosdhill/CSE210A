load ../../harness

@test "ebe0c1a3c99b" {
  check 'skip  ; 
 z := x    + -2' '⇒ z := (x+-2), {}
⇒ skip, {z → -2}'
}
