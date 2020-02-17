load ../../harness

@test "e4780e477d2a" {
  check 'if (true   ∧   z   *     3    =     y    +    -3)    then 
 
 skip      else skip  ' '⇒ skip, {}'
}
