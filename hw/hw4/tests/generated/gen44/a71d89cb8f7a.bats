load ../../harness

@test "a71d89cb8f7a" {
  check 'x:=   x   +   2    ;skip' '⇒ skip; skip, {x → 2}
⇒ skip, {x → 2}'
}
