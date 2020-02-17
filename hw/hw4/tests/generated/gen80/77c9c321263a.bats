load ../../harness

@test "77c9c321263a" {
  check 'if (false  ∨ -4   +z     =z-  z)  then   
skip  else skip  ' '⇒ skip, {}'
}
