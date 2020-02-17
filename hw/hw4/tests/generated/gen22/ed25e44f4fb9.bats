load ../../harness

@test "ed25e44f4fb9" {
  check 'if (y     + T     =    x    *   z     ∨   false)    then 
u:= 2    *-3  else y   :=    kb     +     C2  ' '⇒ u := (2*-3), {}
⇒ skip, {u → -6}'
}
