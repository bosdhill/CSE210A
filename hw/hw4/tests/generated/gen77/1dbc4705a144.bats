load ../../harness

@test "1dbc4705a144" {
  check 'if (false     ∨z     * y =   x     +    -4)  then 



skip  else T:=   -4 + 1   ' '⇒ T := (-4+1), {}
⇒ skip, {T → -3}'
}
