load ../../harness

@test "92ec97e39b29" {
  check 'if (true  ∨     4  +    4    =  3    -x)   then skip  else   
  skip     ' '⇒ skip, {}'
}
