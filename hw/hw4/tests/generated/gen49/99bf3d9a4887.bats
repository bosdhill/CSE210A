load ../../harness

@test "99bf3d9a4887" {
  check 'if (¬(1     +  y<2     + t))   then  
skip  else 
 x     :=-2   +   x     ' '⇒ x := (-2+x), {}
⇒ skip, {x → -2}'
}
