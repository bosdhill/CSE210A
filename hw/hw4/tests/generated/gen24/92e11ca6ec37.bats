load ../../harness

@test "92e11ca6ec37" {
  check 'skip   ;  x    :=     z*  4  ' '⇒ x := (z*4), {}
⇒ skip, {x → 0}'
}
