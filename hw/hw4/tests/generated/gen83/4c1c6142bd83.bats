load ../../harness

@test "4c1c6142bd83" {
  check 'skip  ;y   :=     -3   -  y  ' '⇒ y := (-3-y), {}
⇒ skip, {y → -3}'
}
