load ../../harness

@test "fbdb9076c5c0" {
  check 'while y -x  =     -3 ∧  false      do  skip   ' '⇒ skip, {}'
}
