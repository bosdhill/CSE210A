load ../../harness

@test "e66112c56b47" {
  check 'while z     *-4   =  -4  *  t  ∧  false    do   y    :=   -2   + 2 ' '⇒ skip, {}'
}
