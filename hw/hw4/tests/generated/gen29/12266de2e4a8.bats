load ../../harness

@test "12266de2e4a8" {
  check 'C:=   -2  *     x ;

 x     :=   -4 * z' '⇒ skip; x := (-4*z), {C → 0}
⇒ x := (-4*z), {C → 0}
⇒ skip, {C → 0, x → 0}'
}
