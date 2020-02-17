load ../../harness

@test "501b71dcee02" {
  check 'z    :=   z -  y   ' '⇒ skip, {z → 0}'
}
