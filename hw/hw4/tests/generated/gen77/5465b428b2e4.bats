load ../../harness

@test "5465b428b2e4" {
  check 'if (y=x+  2∧    true)     then   skip     else skip' '⇒ skip, {}'
}
