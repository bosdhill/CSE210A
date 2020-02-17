load ../../harness

@test "c5bd56a777d0" {
  check 'if (1   =   x   -    z ∧    D *    y    <  q4     -   1) then  

 y:=    y      else   y :=   1     ' '⇒ y := 1, {}
⇒ skip, {y → 1}'
}
