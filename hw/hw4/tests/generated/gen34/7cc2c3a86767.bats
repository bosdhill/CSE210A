load ../../harness

@test "7cc2c3a86767" {
  check 'if (3  -  z=    -2 +4)      then skip     else  
z  :=    -1*    4 ' '⇒ z := (-1*4), {}
⇒ skip, {z → -4}'
}
