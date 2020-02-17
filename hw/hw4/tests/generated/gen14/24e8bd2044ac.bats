load ../../harness

@test "24e8bd2044ac" {
  check 'while true  ∧false    do 
z    :=x    +   1   ' '⇒ skip, {}'
}
