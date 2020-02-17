load ../../harness

@test "4549c9c0fd37" {
  check 'if (x    +     2    =y * y  ∨ l4   <0)  then z   :=  4 +  z else 
n     :=    y    *     0 ' '⇒ n := (y*0), {}
⇒ skip, {n → 0}'
}
