load ../../harness

@test "00b3f1b643ae" {
  check 'if (false ∧   true)    then   skip      else   
skip   ' '⇒ skip, {}'
}
