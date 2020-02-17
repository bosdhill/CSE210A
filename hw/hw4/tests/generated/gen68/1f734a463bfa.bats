load ../../harness

@test "1f734a463bfa" {
  check 'if (z +4 <   -1    ∨   true)  then  
skip  else  
skip  ' '⇒ skip, {}'
}
