load ../../harness

@test "6917260cf0dd" {
  check 'if (¬false)   then 

  skip   else  skip ' '⇒ skip, {}'
}
