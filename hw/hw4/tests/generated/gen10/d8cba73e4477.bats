load ../../harness

@test "d8cba73e4477" {
  check 'y :=  -3   + z' '⇒ skip, {y → -3}'
}
