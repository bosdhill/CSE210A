load ../../harness

@test "ec3b2fdd00a0" {
  check 'I:=  0    -    y     ;
   
pg :=  y    *     y  ' '⇒ skip; pg := (y*y), {I → 0}
⇒ pg := (y*y), {I → 0}
⇒ skip, {I → 0, pg → 0}'
}
