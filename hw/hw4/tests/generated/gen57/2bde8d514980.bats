load ../../harness

@test "2bde8d514980" {
  check 'if (¬(2- x  =     4    *  -4))    then  
 y   :=x -     -4   else x :=     -4   -  z    ' '⇒ y := (x--4), {}
⇒ skip, {y → 4}'
}
