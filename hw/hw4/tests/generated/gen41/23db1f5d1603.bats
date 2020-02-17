load ../../harness

@test "23db1f5d1603" {
  check 'y  := W   -   z; skip' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
