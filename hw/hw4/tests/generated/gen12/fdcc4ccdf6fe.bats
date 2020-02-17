load ../../harness

@test "fdcc4ccdf6fe" {
  check 'if (¬(x    +y   =   x*   0))      then  
 
z   :=4    +    -3      else  

 y    :=     -4     *     -4   ' '⇒ y := (-4*-4), {}
⇒ skip, {y → 16}'
}
