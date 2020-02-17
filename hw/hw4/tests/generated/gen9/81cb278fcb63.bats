load ../../harness

@test "81cb278fcb63" {
  check 'if (0     -   z   = x     * -2   ∨x *   1  =  -2*    y)  then  
skip   else    
x    :=  NP*    z' '⇒ skip, {}'
}
