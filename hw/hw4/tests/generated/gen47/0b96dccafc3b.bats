load ../../harness

@test "0b96dccafc3b" {
  check 'if (false   ∧     true)  then  skip      else 
 skip' '⇒ skip, {}'
}
