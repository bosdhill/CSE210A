load ../../harness

@test "69b73a60b5dd" {
  check 'if (¬(x     - 4 =   y     -     x)) then skip  else skip     ' '⇒ skip, {}'
}
