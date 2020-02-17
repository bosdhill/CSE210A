load ../../harness

@test "6bf74c9bdb0d" {
  check 'x :=x + a    ;  skip  ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
