load ../../harness

@test "52abe1024c41" {
  check 'if (¬(y    +   e  =   -4 + -3))    then z:=  -2  +   y  else z := x -  -4   ' '⇒ z := (-2+y), {}
⇒ skip, {z → -2}'
}
