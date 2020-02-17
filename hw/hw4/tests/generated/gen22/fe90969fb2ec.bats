load ../../harness

@test "fe90969fb2ec" {
  check 'x    :=    Y5  *   -4  ;
 x     := z*x     ' '⇒ skip; x := (z*x), {x → 0}
⇒ x := (z*x), {x → 0}
⇒ skip, {x → 0}'
}
