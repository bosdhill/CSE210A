load ../../harness

@test "abdc117f5d41" {
  check 'if (false     ∧     false)  then 
  r:=y*  0   else 
    x:=-3   +     -1  ' '⇒ x := (-3+-1), {}
⇒ skip, {x → -4}'
}
