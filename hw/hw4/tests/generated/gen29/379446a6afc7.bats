load ../../harness

@test "379446a6afc7" {
  check 'z :=    x     +     x    ;
  
x:= -4   -  x     ' '⇒ skip; x := (-4-x), {z → 0}
⇒ x := (-4-x), {z → 0}
⇒ skip, {x → -4, z → 0}'
}
