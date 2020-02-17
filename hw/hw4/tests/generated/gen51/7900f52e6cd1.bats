load ../../harness

@test "7900f52e6cd1" {
  check 'if (false     ∨false)      then   
 x:= 4 + y     else skip     ' '⇒ skip, {}'
}
