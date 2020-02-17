load ../../harness

@test "d6811393ace6" {
  check 'x     :=     3 +    -2;
skip' '⇒ skip; skip, {x → 1}
⇒ skip, {x → 1}'
}
