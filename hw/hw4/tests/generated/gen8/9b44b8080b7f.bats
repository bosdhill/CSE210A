load ../../harness

@test "9b44b8080b7f" {
  check 'if (true∨ x    *     Eb   = 1)  then skip   else skip  ' '⇒ skip, {}'
}
