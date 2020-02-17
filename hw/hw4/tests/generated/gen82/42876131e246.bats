load ../../harness

@test "42876131e246" {
  check 'if (¬false) then    skip      else   skip    ' '⇒ skip, {}'
}
