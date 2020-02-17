load ../../harness

@test "ae190aaf350d" {
  check 'z    :=x*z  ' '⇒ skip, {z → 0}'
}
