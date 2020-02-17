load ../../harness

@test "0754ae4d553d" {
  check 'y   :=     x  +  x   ' '⇒ skip, {y → 0}'
}
