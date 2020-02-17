load ../../harness

@test "ecf0b4c3eaad" {
  check 'if (0    =   -2 +    3  ∨    true)    then 
skip    else PL     :=  x     - y     ' '⇒ skip, {}'
}
