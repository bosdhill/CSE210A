load ../../harness

@test "61d5e31db1f7" {
  check 'if (¬true)     then 



skip    else 
z:=   -1    * 2' '⇒ z := (-1*2), {}
⇒ skip, {z → -2}'
}
