load ../../harness

@test "5010b6fe40dd" {
  check 'if (false  ∨    true)  then   
x  :=  -2-   z else    
 S   :=     2  *  1     ' '⇒ x := (-2-z), {}
⇒ skip, {x → -2}'
}
