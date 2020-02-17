load ../../harness

@test "b8f0ace420bb" {
  check 'if (-4    +   3     <    y    * 0∧   false)      then  
skip    else 
  skip' '⇒ skip, {}'
}
