load ../../harness

@test "dbc4f1dab963" {
  check 'while false     ∧x +y     <   4     +     0      do 
x   :=  y  -    s9' '⇒ skip, {}'
}
