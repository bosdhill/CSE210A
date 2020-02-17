load ../../harness

@test "aac0ff043a53" {
  check 'if (false     ∧    -4    -  x   =    K    *     -2)     then   skip      else skip  ' '⇒ skip, {}'
}
