load ../../harness

@test "393a0217a8b7" {
  check 'if (2 +     x = 2 +  -2     ∧ true)      then   

 skip     else skip' '⇒ skip, {}'
}
