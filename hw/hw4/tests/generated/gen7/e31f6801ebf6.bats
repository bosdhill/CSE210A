load ../../harness

@test "e31f6801ebf6" {
  check 'if (-4   * -4     =  x    *1 ∨ y < z  -     2) then 
skip    else 
   
x:=  3 -     z ' '⇒ x := (3-z), {}
⇒ skip, {x → 3}'
}
