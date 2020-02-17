load ../../harness

@test "ff22210341f6" {
  check 'if (z     - y <   y-     x   ∨ true) then    skip    else skip ' '⇒ skip, {}'
}
