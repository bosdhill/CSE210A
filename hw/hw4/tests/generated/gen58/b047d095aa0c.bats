load ../../harness

@test "b047d095aa0c" {
  check 'z  :=  z  * -4  ' '⇒ skip, {z → 0}'
}
