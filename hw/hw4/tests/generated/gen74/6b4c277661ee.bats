load ../../harness

@test "6b4c277661ee" {
  check 'y    :=   y    *     z     ;skip' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
