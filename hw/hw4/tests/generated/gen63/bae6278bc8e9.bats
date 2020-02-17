load ../../harness

@test "bae6278bc8e9" {
  check 'y   :=  z' '⇒ skip, {y → 0}'
}
