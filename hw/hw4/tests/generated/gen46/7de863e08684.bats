load ../../harness

@test "7de863e08684" {
  check 'if (¬false)   then  skip     else x     :=    -1   +  y' '⇒ skip, {}'
}
