load ../../harness

@test "08740a60c098" {
  check 'if (¬false)   then   skip   else    skip  ' '⇒ skip, {}'
}
