load ../../harness

@test "62077232750c" {
  check 'if (-2 +  x     =   y    *     L)    then  
  skip     else  skip  ' '⇒ skip, {}'
}
