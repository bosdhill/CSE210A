load ../../harness

@test "fc55e81cdd10" {
  check 'while true  ∧    false     do 
 skip' '⇒ skip, {}'
}
