load ../../harness

@test "2ddfffb9b7d4" {
  check 'y:=   z   -    z  ' '⇒ skip, {y → 0}'
}
