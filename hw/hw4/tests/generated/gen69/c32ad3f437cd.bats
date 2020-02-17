load ../../harness

@test "c32ad3f437cd" {
  check 'z    :=y     -    z' '⇒ skip, {z → 0}'
}
