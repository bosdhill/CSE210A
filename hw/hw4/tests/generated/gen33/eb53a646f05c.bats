load ../../harness

@test "eb53a646f05c" {
  check 'if (4   +     0    = 2 +   DA    ∨    2*  1 =   x  - 0)   then 
skip   else 
skip     ' '⇒ skip, {}'
}
