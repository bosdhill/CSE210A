load ../../harness

@test "64a5d2839f5c" {
  check 'if (false∧   4 -    -4  < 3  +     y)   then  
 

x  :=    3   +   -1    else 
 
y:=   4   *   -2   ' '⇒ y := (4*-2), {}
⇒ skip, {y → -8}'
}
