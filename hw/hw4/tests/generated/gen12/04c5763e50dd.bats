load ../../harness

@test "04c5763e50dd" {
  check 'x    :=     n  ;

h:=  x ' '⇒ skip; h := x, {x → 0}
⇒ h := x, {x → 0}
⇒ skip, {h → 0, x → 0}'
}
