load ../../harness

@test "42722d8bfd24" {
  check 'if (false   ∧     true)   then  skip      else  
 skip' '⇒ skip, {}'
}
