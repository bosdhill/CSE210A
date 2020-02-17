load ../../harness

@test "c0e4684a2e8c" {
  check 'skip   ;
if (¬(1  -  y  =   -4* x))     then y:=    -3+ y  else  z  :=  3*    2    ' '⇒ if ¬((1-y)=(-4*x)) then { y := (-3+y) } else { z := (3*2) }, {}
⇒ y := (-3+y), {}
⇒ skip, {y → -3}'
}
