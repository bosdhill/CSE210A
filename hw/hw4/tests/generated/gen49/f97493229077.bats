load ../../harness

@test "f97493229077" {
  check 'R :=   0    +  y     ;skip' '⇒ skip; skip, {R → 0}
⇒ skip, {R → 0}'
}
