load ../../harness

@test "fca824cb9bb2" {
  check 'if (¬true)      then 

x   :=     x   -y  else 
  x     :=    -3   +  0' '⇒ x := (-3+0), {}
⇒ skip, {x → -3}'
}
