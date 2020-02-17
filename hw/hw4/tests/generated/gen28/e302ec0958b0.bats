load ../../harness

@test "e302ec0958b0" {
  check 'while false    do 
  x  :=   x +4    ' '⇒ skip, {}'
}
