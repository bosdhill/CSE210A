load ../../harness

@test "e1e1995c9c2d" {
  check 'z    :=   1  ; x  :=    -1    -   -4 ;
x     :=    -3  +  y     ' '⇒ skip; x := (-1--4); x := (-3+y), {z → 1}
⇒ x := (-1--4); x := (-3+y), {z → 1}
⇒ skip; x := (-3+y), {x → 3, z → 1}
⇒ x := (-3+y), {x → 3, z → 1}
⇒ skip, {x → -3, z → 1}'
}
