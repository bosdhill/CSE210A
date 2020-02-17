load ../../harness

@test "cac700157e31" {
  check 'if (true  ∨     true)  then 
Xv :=     RW   -   y    else  z   :=x -   2   ' '⇒ Xv := (RW-y), {}
⇒ skip, {Xv → 0}'
}
