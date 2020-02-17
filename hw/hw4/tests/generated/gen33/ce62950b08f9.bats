load ../../harness

@test "ce62950b08f9" {
  check 'if false     then j0    :=  -4 *   2  else skip   ' '⇒ skip, {}'
}
