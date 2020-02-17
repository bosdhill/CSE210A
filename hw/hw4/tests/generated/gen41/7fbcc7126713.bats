load ../../harness

@test "7fbcc7126713" {
  check 'y:= x    -  z  ' '⇒ skip, {y → 0}'
}
