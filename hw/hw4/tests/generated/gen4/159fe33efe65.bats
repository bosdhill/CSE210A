load ../../harness

@test "159fe33efe65" {
  check 'y    := z  *  Zs ' '⇒ skip, {y → 0}'
}
