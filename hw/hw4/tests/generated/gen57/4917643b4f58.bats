load ../../harness

@test "4917643b4f58" {
  check 'if (false ∧    fy=    4   *     -4)      then  
 y     :=    -3    +x   else 
   x :=  y -x   ' '⇒ x := (y-x), {}
⇒ skip, {x → 0}'
}
