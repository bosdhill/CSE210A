load ../../harness

@test "4fc2f84eded9" {
  check 'if (¬(x -   y     < -2   +   -3))    then  skip  else 
skip    ' '⇒ skip, {}'
}
