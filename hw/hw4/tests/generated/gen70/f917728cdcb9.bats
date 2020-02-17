load ../../harness

@test "f917728cdcb9" {
  check 'A5 := 4  +2 ;
 
 
x :=    z   -  -2' '⇒ skip; x := (z--2), {A5 → 6}
⇒ x := (z--2), {A5 → 6}
⇒ skip, {A5 → 6, x → 2}'
}
