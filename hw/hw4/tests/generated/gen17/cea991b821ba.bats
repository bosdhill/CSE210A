load ../../harness

@test "cea991b821ba" {
  check 'if (¬(1     *    y    =  1))    then 
skip else  
 
y   :=  x     + 1  ' '⇒ skip, {}'
}
