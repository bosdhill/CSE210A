load ../../harness

@test "e0517cee3de7" {
  check 'if (¬(x    -     3  < y   +  x))     then  
skip   else x := z -x   ' '⇒ x := (z-x), {}
⇒ skip, {x → 0}'
}
