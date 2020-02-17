load ../../harness

@test "04a652cd9afd" {
  check 'while true ∧ false do 

 y   := -1 -  x' '⇒ skip, {}'
}
