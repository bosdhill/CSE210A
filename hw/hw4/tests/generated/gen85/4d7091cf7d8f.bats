load ../../harness

@test "4d7091cf7d8f" {
  check 'x:=  y  +z     ;
  

skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
