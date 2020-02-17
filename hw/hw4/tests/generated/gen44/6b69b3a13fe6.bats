load ../../harness

@test "6b69b3a13fe6" {
  check 'skip     ;y     :=   y+  -4 ' '⇒ y := (y+-4), {}
⇒ skip, {y → -4}'
}
