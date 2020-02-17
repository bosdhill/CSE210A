load ../../harness

@test "11295d81e007" {
  check 'while false     ∨   false   do  
 x  :=    2  + 0' '⇒ skip, {}'
}
