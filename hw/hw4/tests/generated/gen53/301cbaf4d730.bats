load ../../harness

@test "301cbaf4d730" {
  check 'x:=  y; x   :=   -2+   1     ' '⇒ skip; x := (-2+1), {x → 0}
⇒ x := (-2+1), {x → 0}
⇒ skip, {x → -1}'
}
