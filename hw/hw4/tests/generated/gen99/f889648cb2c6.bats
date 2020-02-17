load ../../harness

@test "f889648cb2c6" {
  check 'if (false   ∧ y     *z    = y+y)      then   
skip else 
skip  ' '⇒ skip, {}'
}
