load ../../harness

@test "5f23c3891741" {
  check 'z :=   z ;skip     ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
