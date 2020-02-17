load ../../harness

@test "4951749a395a" {
  check 'if (false  ∨    z   -    -3 <    z *   0)    then  

skip   else 


z:=    4    *  -3    ' '⇒ z := (4*-3), {}
⇒ skip, {z → -12}'
}
