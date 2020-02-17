load ../../harness

@test "e8e5fdf68097" {
  check 'if (4    +     z=  z +  4∧    z   -     -2    = -1    *   a7) then y     :=   0     +  -1 else 
z  :=  y   +    -3   ' '⇒ z := (y+-3), {}
⇒ skip, {z → -3}'
}
