load ../../harness

@test "d61ccada82a7" {
  check 'y :=   x  +   x ;

  x    :=    -2     *    y  ' '⇒ skip; x := (-2*y), {y → 0}
⇒ x := (-2*y), {y → 0}
⇒ skip, {x → 0, y → 0}'
}
