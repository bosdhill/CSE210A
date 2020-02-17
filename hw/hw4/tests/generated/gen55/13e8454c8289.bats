load ../../harness

@test "13e8454c8289" {
  check 'while z   +     3= x   *   -2   ∧    true  do 
 z   := -4    +    y     ' '⇒ skip, {}'
}
