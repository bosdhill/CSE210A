load ../../harness

@test "ca2515f670b9" {
  check 'if (y    * -2<2  * 1)    then 
   
z :=y  * 4  else  x := -2    *x' '⇒ z := (y*4), {}
⇒ skip, {z → 0}'
}
