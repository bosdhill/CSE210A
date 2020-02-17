load ../../harness

@test "866b844ecd61" {
  check 'x:=  y- x;  
skip    ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
