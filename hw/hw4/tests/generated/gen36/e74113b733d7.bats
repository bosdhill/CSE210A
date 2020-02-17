load ../../harness

@test "e74113b733d7" {
  check 'g4:=    x   ; 
 
s   :=     -1    -   q    ' '⇒ skip; s := (-1-q), {g4 → 0}
⇒ s := (-1-q), {g4 → 0}
⇒ skip, {g4 → 0, s → -1}'
}
