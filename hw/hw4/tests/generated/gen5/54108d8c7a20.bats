load ../../harness

@test "54108d8c7a20" {
  check 'if (¬(-1   -     x     =     y  +   4))     then 
 x  :=     4    *     3  else   

 skip' '⇒ x := (4*3), {}
⇒ skip, {x → 12}'
}
