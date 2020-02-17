load ../../harness

@test "4629cba19033" {
  check 'if (¬(z     -    -3 =  3- z))    then  skip     else 
   z   :=    -4  -     -2' '⇒ z := (-4--2), {}
⇒ skip, {z → -2}'
}
