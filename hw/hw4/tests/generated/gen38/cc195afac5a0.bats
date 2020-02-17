load ../../harness

@test "cc195afac5a0" {
  check 'y     := J0  *    4    +-3   ;    
Kc     :=   y   +   b    ' '⇒ skip; Kc := (y+b), {y → -3}
⇒ Kc := (y+b), {y → -3}
⇒ skip, {Kc → -3, y → -3}'
}
