load ../../harness

@test "f3954163cc48" {
  check 'if (x    * 2    =  x+     x   ∧   2   - 3  <     z)      then  

j := 0+  4   else  
skip   ' '⇒ j := (0+4), {}
⇒ skip, {j → 4}'
}
