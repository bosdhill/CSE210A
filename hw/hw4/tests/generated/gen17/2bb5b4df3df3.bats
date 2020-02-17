load ../../harness

@test "2bb5b4df3df3" {
  check 'skip; x:=  x    + x  ' '⇒ x := (x+x), {}
⇒ skip, {x → 0}'
}
