load ../../harness

@test "833647e4927d" {
  check 'y    :=  z     -  3;skip  ' '⇒ skip; skip, {y → -3}
⇒ skip, {y → -3}'
}
