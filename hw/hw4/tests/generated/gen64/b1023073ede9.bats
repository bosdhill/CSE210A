load ../../harness

@test "b1023073ede9" {
  check 'if (z  -x=    z    +    3∨    4    *    -3     < dz    -K)    then 

z:=   z*    z  else skip   ' '⇒ z := (z*z), {}
⇒ skip, {z → 0}'
}
