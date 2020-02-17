load ../../harness

@test "b6471e54d3a7" {
  check 'if true      then  skip  else  
skip  ' '⇒ skip, {}'
}
