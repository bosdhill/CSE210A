load ../../harness

@test "3422deadf53d" {
  check 'if (¬(z-     1     =    1)) then  skip  else skip ' '⇒ skip, {}'
}
