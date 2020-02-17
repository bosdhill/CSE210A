load ../../harness

@test "f6793ee3aab1" {
  check 'y     :=x  ; 
uo     :=    2     ' '⇒ skip; uo := 2, {y → 0}
⇒ uo := 2, {y → 0}
⇒ skip, {uo → 2, y → 0}'
}
