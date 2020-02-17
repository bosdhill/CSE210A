load ../../harness

@test "15ccaa1d3d69" {
  check 'if (¬(z     -    2   < 3*x))     then  
skip  else   
 y:=   4*  z' '⇒ y := (4*z), {}
⇒ skip, {y → 0}'
}
