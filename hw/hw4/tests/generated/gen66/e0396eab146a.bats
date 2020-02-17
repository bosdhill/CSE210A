load ../../harness

@test "e0396eab146a" {
  check 'if (4   -     -3    =     y+3∨true) then   skip   else  skip' '⇒ skip, {}'
}
