load ../../harness

@test "ae0468ac2ce9" {
  check 'if (x *     y  < y    +ZS     ∧  y*    z    =z -   -2)      then  
 

y :=   y  *    z     else x   := y  --1     ' '⇒ x := (y--1), {}
⇒ skip, {x → 1}'
}
