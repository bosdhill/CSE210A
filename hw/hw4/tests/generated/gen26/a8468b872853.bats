load ../../harness

@test "a8468b872853" {
  check 'y   :=   vi -y   ;    x     :=     -4+ x' '⇒ skip; x := (-4+x), {y → 0}
⇒ x := (-4+x), {y → 0}
⇒ skip, {x → -4, y → 0}'
}
