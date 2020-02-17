load ../../harness

@test "2dd632c48f49" {
  check 'skip ;
  x := z -y' '⇒ x := (z-y), {}
⇒ skip, {x → 0}'
}
