load ../../harness

@test "98af9367d18e" {
  check 'if (x    +1<w  -  z     ∧    true)    then  
   z :=   -3    *     tq     else 
x   :=  3    *   -2   ' '⇒ x := (3*-2), {}
⇒ skip, {x → -6}'
}
