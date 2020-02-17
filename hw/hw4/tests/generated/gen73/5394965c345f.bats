load ../../harness

@test "5394965c345f" {
  check 'z  := y    *  -2;  

 x := -1+  x  ' '⇒ skip; x := (-1+x), {z → 0}
⇒ x := (-1+x), {z → 0}
⇒ skip, {x → -1, z → 0}'
}
