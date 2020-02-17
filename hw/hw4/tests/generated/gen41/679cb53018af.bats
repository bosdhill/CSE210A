load ../../harness

@test "679cb53018af" {
  check 'x    := x   + y    ;   skip   ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
