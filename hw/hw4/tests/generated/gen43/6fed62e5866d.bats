load ../../harness

@test "6fed62e5866d" {
  check 'y   :=     z  * y' '⇒ skip, {y → 0}'
}
