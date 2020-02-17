load ../../harness

@test "9fe90c68609f" {
  check 'z:= y* x   ' '⇒ skip, {z → 0}'
}
