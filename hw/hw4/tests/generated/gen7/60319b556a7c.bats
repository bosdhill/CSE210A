load ../../harness

@test "60319b556a7c" {
  check 'z     :=     -1    -    z' '⇒ skip, {z → -1}'
}
