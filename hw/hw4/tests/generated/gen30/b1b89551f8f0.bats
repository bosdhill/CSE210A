load ../../harness

@test "b1b89551f8f0" {
  check 'z  :=x *  x  ; 


x  :=   2 *  3    ' '⇒ skip; x := (2*3), {z → 0}
⇒ x := (2*3), {z → 0}
⇒ skip, {x → 6, z → 0}'
}
