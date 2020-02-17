load ../../harness

@test "e526357c3dbc" {
  check 'z     :=     x  +    -1   ' '⇒ skip, {z → -1}'
}
