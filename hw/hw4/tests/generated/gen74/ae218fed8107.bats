load ../../harness

@test "ae218fed8107" {
  check 'z  :=  z    + 2 ' '⇒ skip, {z → 2}'
}
