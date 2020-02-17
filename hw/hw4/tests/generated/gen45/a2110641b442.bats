load ../../harness

@test "a2110641b442" {
  check 'x:= JO  +3 ;
 
x :=     lx     ' '⇒ skip; x := lx, {x → 3}
⇒ x := lx, {x → 3}
⇒ skip, {x → 0}'
}
