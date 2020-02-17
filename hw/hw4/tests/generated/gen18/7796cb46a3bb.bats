load ../../harness

@test "7796cb46a3bb" {
  check 'if (¬(z    +  z =    z   +     x))     then j    :=   z  *  z      else 
 y     :=  -1 -  3  ' '⇒ y := (-1-3), {}
⇒ skip, {y → -4}'
}
