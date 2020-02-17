load ../../harness

@test "e92bfd8a33d0" {
  check 'while -4  *    -2 <     u6     *    1   ∨ ¬true   do z    :=    -3    + -3     ' '⇒ skip, {}'
}
