load ../../harness

@test "07c057d53ed6" {
  check 'while y   +    z     < -1     *  z  do {skip  ;   
skip}' '⇒ skip, {}'
}
