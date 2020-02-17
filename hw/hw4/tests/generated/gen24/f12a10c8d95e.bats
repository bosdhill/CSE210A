load ../../harness

@test "f12a10c8d95e" {
  check 'if (¬false)      then   skip     else 
skip     ' '⇒ skip, {}'
}
