load ../../harness

@test "832ae942a2d9" {
  check 'y     :=   z    + K8' '⇒ skip, {y → 0}'
}
