load ../../harness

@test "de51b2af12d6" {
  check 'x  :=M   +  x  ;skip ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
