load ../../harness

@test "eae8fc701e4e" {
  check 'if (true   ∧false)      then 

skip      else   skip   ' '⇒ skip, {}'
}
