load ../../harness

@test "43db5402e4dc" {
  check 'if (¬(3    +     x   <   x     +    x))    then  skip   else 
skip   ' '⇒ skip, {}'
}
