load ../../harness

@test "29044f729bbd" {
  check 'while false     do 
 x   :=    x    * x' '⇒ skip, {}'
}
