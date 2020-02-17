load ../../harness

@test "7a7cbd08eb8c" {
  check 'if (¬true)      then skip else 
  x := x   *     4 ' '⇒ x := (x*4), {}
⇒ skip, {x → 0}'
}
