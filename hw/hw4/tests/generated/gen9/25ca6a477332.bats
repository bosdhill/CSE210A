load ../../harness

@test "25ca6a477332" {
  check 'skip ;f    :=0  - x ' '⇒ f := (0-x), {}
⇒ skip, {f → 0}'
}
