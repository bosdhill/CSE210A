load ../../harness

@test "dc34298a4bb4" {
  check 'if (0    +x = -2 -    y ∨   y    +     x=     1   -y)    then 
  x :=  OK     +  z      else   
 z  :=     z     *  x ' '⇒ z := (z*x), {}
⇒ skip, {z → 0}'
}
