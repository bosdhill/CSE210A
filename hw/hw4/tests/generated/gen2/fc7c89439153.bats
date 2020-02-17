load ../../harness

@test "fc7c89439153" {
  check 'skip  ;x    :=  x  *    -4 ' '⇒ x := (x*-4), {}
⇒ skip, {x → 0}'
}
