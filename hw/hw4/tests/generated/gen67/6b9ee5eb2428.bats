load ../../harness

@test "6b9ee5eb2428" {
  check 'z    :=  y    -y    ' '⇒ skip, {z → 0}'
}
