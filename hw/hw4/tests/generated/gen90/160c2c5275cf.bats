load ../../harness

@test "160c2c5275cf" {
  check 'x  :=   y-     z     ' '⇒ skip, {x → 0}'
}
