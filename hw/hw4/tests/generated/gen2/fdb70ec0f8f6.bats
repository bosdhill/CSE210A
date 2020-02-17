load ../../harness

@test "fdb70ec0f8f6" {
  check 'x    :=    4    *zP ' '⇒ skip, {x → 0}'
}
