load ../../harness

@test "d5c621dc549c" {
  check 'if (true   ∧   -1+  0   =   -1  *  z)  then   
  skip else bX:=    -1   +  z  ' '⇒ bX := (-1+z), {}
⇒ skip, {bX → -1}'
}
