load ../../harness

@test "b876d403958e" {
  check 'if (x*3    <     z - 3  ∨     1   * -2  =Yx*   4)      then  
skip else 
 z:=x   -  3   ' '⇒ z := (x-3), {}
⇒ skip, {z → -3}'
}
