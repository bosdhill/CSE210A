load ../../harness

@test "18d48b788b75" {
  check 'if (¬(z +    2     =-4  + -2))  then  y    :=    -4-  4 else 
 z :=  2    * 4   ' '⇒ y := (-4-4), {}
⇒ skip, {y → -8}'
}
