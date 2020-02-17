load ../../harness

@test "48cb1e945e5b" {
  check 'x    := 3    -     3;


skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
