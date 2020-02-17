load ../../harness

@test "b81c7305da02" {
  check 'y     :=x   *     2' '⇒ skip, {y → 0}'
}
