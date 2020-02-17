load ../../harness

@test "573d8801b1bc" {
  check 'if (x*   y  <  3  -  -2     ∧    false)      then  
skip     else  skip  ' '⇒ skip, {}'
}
