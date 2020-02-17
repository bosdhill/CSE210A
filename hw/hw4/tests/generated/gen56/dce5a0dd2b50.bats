load ../../harness

@test "dce5a0dd2b50" {
  check 'while true  ∧   false do 
skip    ' '⇒ skip, {}'
}
