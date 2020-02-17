load ../../harness

@test "3d754b076249" {
  check 'x:=   x+  -2   ;

 skip' '⇒ skip; skip, {x → -2}
⇒ skip, {x → -2}'
}
