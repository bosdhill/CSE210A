load ../../harness

@test "c3186f0476a2" {
  check 'skip;   x   := 0     *-1' '⇒ x := (0*-1), {}
⇒ skip, {x → 0}'
}
