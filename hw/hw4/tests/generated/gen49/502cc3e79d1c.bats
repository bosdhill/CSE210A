load ../../harness

@test "502cc3e79d1c" {
  check 'L0:=    -3   *  3 ;
  
 x   :=    z    *x  ' '⇒ skip; x := (z*x), {L0 → -9}
⇒ x := (z*x), {L0 → -9}
⇒ skip, {L0 → -9, x → 0}'
}
