load ../../harness

@test "be25d2c6aa49" {
  check 'y:=   z  -  -1    ' '⇒ skip, {y → 1}'
}
