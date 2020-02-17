load ../../harness

@test "33c8416d8ceb" {
  check 'if (¬(3*     x =     y   *     -4))   then  
  skip else 
  
 x:=-1   *-4  ' '⇒ x := (-1*-4), {}
⇒ skip, {x → 4}'
}
