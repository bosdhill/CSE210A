load ../../harness

@test "30c33fd22b83" {
  check 'if (x -     4 <1     *   4     ∧    false) then  
 
skip    else  
x  :=    4 +  -4 ' '⇒ x := (4+-4), {}
⇒ skip, {x → 0}'
}
