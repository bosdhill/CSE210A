load ../../harness

@test "35dca15ee859" {
  check 'if (true ∨    1  - -1  <   z  +  x)      then 
 
x    :=-3  +    3      else     
y  :=  3-  2     ' '⇒ x := (-3+3), {}
⇒ skip, {x → 0}'
}
