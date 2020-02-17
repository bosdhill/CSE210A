load ../../harness

@test "6959a843c4d9" {
  check 'if (¬false)     then   skip   else skip  ' '⇒ skip, {}'
}
