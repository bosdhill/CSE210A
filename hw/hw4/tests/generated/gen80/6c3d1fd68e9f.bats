load ../../harness

@test "6c3d1fd68e9f" {
  check 'if (y*  z     <     1  +   4∧   0*     0=     2    *     z)     then 
  x    :=   x +  z  else     
skip ' '⇒ x := (x+z), {}
⇒ skip, {x → 0}'
}
