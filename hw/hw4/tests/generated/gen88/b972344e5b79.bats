load ../../harness

@test "b972344e5b79" {
  check 'if (true ∧    true)    then    
 m   :=    y    *z   else 

z     := 0   *    z   ' '⇒ m := (y*z), {}
⇒ skip, {m → 0}'
}
