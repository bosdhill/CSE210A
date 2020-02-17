load ../../harness

@test "dd6876ec57b6" {
  check 'while false   ∧   false     do 
skip    ' '⇒ skip, {}'
}
