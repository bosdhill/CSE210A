load ../../harness

@test "b996e27dc2a6" {
  check 'if (y+     y   <    -2     *   z   ∨ -2    +   z  = x   *  -2)     then  
skip   else  
 skip   ' '⇒ skip, {}'
}
