load ../../harness

@test "7ce9330088b2" {
  check 'while false∧  false   do z:=  -2    *  y     ' '⇒ skip, {}'
}
