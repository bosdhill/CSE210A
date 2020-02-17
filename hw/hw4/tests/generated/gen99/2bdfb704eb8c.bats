load ../../harness

@test "2bdfb704eb8c" {
  check 'skip     ;y     := y -     z  ' '⇒ y := (y-z), {}
⇒ skip, {y → 0}'
}
