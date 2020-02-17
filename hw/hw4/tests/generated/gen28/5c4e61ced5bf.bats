load ../../harness

@test "5c4e61ced5bf" {
  check 'if (¬(-3     *  -3<     4    - 4)) then 
  
x:=    2   +    1   else 
x:=  x     +  1' '⇒ x := (2+1), {}
⇒ skip, {x → 3}'
}
