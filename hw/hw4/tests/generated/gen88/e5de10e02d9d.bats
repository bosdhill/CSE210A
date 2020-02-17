load ../../harness

@test "e5de10e02d9d" {
  check 'if (s6     =  1    - 3∨  z  *   z  =     -3* x)   then 
y:=  2     *    -1      else Q:=  -4  +  1     ' '⇒ y := (2*-1), {}
⇒ skip, {y → -2}'
}
