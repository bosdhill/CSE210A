load ../../harness

@test "f1ba32c9838e" {
  check 'y :=x9  *     z    ' '⇒ skip, {y → 0}'
}
