load ../../harness

@test "894274e48a9b" {
  check 'if (3     =     4-   y     ∧     true)  then 
skip     else 

   z     := x+   -1  ' '⇒ z := (x+-1), {}
⇒ skip, {z → -1}'
}
