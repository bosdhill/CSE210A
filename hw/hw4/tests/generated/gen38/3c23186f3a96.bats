load ../../harness

@test "3c23186f3a96" {
  check 'skip ;c    := y    +     0 ' '⇒ c := (y+0), {}
⇒ skip, {c → 0}'
}
