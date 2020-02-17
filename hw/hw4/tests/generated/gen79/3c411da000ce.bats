load ../../harness

@test "3c411da000ce" {
  check 'if (¬(x  -  z  <    x -  x))      then  
 z:= -3     -  z    else z:=   y     -    W     ' '⇒ z := (-3-z), {}
⇒ skip, {z → -3}'
}
