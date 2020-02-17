load ../../harness

@test "cd46c0a85f4f" {
  check 'x     :=y *2    ;


skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
