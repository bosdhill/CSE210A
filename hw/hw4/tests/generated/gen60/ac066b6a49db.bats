load ../../harness

@test "ac066b6a49db" {
  check 'skip;
    x  :=  y   +   y  ' '⇒ x := (y+y), {}
⇒ skip, {x → 0}'
}
