load ../../harness

@test "ba73ee3af7f9" {
  check 'z :=  -4 -y ;
x     :=   4    +     x    ' '⇒ skip; x := (4+x), {z → -4}
⇒ x := (4+x), {z → -4}
⇒ skip, {x → 4, z → -4}'
}
