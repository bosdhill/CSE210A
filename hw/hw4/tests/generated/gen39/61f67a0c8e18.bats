load ../../harness

@test "61f67a0c8e18" {
  check 'if (¬(z    +3  <   -3  +    T))      then   skip      else z :=  -4 ' '⇒ skip, {}'
}
