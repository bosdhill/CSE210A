load ../../harness

@test "c8e2e25d592e" {
  check 'if (x +   y =     z  +     4)    then 
skip   else  skip    ' '⇒ skip, {}'
}
