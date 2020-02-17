load ../../harness

@test "19b96412bdcd" {
  check 'while y    *     4<  y   *     -3    ∧    -4    -     y <    4   -     z  do  x   :=z  ' '⇒ skip, {}'
}
