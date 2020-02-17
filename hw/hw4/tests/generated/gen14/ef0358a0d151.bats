load ../../harness

@test "ef0358a0d151" {
  check 'if (x   -   1 =  -4   -   x ∨    false) then 
skip     else 
skip  ' '⇒ skip, {}'
}
