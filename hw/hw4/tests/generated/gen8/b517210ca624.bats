load ../../harness

@test "b517210ca624" {
  check 'skip;

  x:=   4  + -3     ' '⇒ x := (4+-3), {}
⇒ skip, {x → 1}'
}
