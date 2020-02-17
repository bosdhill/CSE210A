load ../../harness

@test "bcb34724c6ab" {
  check 'if (3    *    x     =  4    +  2∧    K*   x    <     x-y)  then 
x:=    z     - 4   else 

 x  :=    -2   *    y    ' '⇒ x := (-2*y), {}
⇒ skip, {x → 0}'
}
