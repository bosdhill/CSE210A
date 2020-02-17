load ../../harness

@test "73725d207c9a" {
  check 'while false    ∨     false  do 
  skip    ' '⇒ skip, {}'
}
