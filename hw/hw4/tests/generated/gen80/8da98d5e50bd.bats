load ../../harness

@test "8da98d5e50bd" {
  check 't :=   x   +  -3   ; 
y  :=    x + 3 ' '⇒ skip; y := (x+3), {t → -3}
⇒ y := (x+3), {t → -3}
⇒ skip, {t → -3, y → 3}'
}
