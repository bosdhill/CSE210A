load ../../harness

@test "97aa072b5dbe" {
  check 'if (false    ∧   -2   + x  <     x     +0) then  
skip     else skip ' '⇒ skip, {}'
}
