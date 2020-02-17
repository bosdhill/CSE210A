load ../../harness

@test "d3450d345c3c" {
  check 'x :=  RK - x;  

 skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
