load ../../harness

@test "9fc9a3b7decb" {
  check 'while x  +  z     <     z*  z  ∧  false   do y    := 0    -  x  ' '⇒ skip, {}'
}
