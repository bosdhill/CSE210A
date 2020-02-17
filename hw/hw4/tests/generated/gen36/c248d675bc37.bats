load ../../harness

@test "c248d675bc37" {
  check 'while false do 
   x   :=  4     -    -3' '⇒ skip, {}'
}
