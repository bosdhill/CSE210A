load ../../harness

@test "ff3e20009a0d" {
  check 'if false    then 

z  :=  -1    + -2    else   x  :=-3 + x     ' '⇒ x := (-3+x), {}
⇒ skip, {x → -3}'
}
