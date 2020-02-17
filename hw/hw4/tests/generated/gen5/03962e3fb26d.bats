load ../../harness

@test "03962e3fb26d" {
  check 'z    :=   3     +  z    ' '⇒ skip, {z → 3}'
}
