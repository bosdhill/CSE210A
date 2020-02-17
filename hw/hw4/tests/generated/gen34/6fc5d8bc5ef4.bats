load ../../harness

@test "6fc5d8bc5ef4" {
  check 'while z -    -2   = x *     -4   ∨    false     do x     :=y    ' '⇒ skip, {}'
}
