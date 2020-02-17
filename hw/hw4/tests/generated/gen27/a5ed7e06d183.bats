load ../../harness

@test "a5ed7e06d183" {
  check 'h     :=x*     4;
  x:=   0' '⇒ skip; x := 0, {h → 0}
⇒ x := 0, {h → 0}
⇒ skip, {h → 0, x → 0}'
}
