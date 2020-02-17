load ../../harness

@test "1d7fc8a4108e" {
  check 'y  :=  z     *     (y   +   x)  ' '⇒ skip, {y → 0}'
}
