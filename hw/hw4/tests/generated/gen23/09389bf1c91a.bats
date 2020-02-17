load ../../harness

@test "09389bf1c91a" {
  check 'if (¬false)  then n:=4  *-1  else  x  :=   x    +  x    ' '⇒ n := (4*-1), {}
⇒ skip, {n → -4}'
}
