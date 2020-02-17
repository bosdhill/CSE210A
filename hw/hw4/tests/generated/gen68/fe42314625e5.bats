load ../../harness

@test "fe42314625e5" {
  check 'if (x     - -1 = -4  +z)   then skip      else  skip ' '⇒ skip, {}'
}
