load ../../harness

@test "6dd8a44472bb" {
  check 'if (-1     +  -1  =   x  +    3  ∧  true)    then  
z :=3 +-1    else  skip    ' '⇒ skip, {}'
}
