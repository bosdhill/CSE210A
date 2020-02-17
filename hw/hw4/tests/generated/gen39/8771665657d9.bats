load ../../harness

@test "8771665657d9" {
  check 'x :=   a   - y  ;
 skip    ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
