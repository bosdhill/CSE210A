load ../../harness

@test "b824e241c994" {
  check 'if (¬false)   then   skip    else  skip   ' '⇒ skip, {}'
}
