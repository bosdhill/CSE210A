load ../../harness

@test "c571ddc89cd6" {
  check 'F7   :=   I -  x     ; 
 y  :=  -3+3' '⇒ skip; y := (-3+3), {F7 → 0}
⇒ y := (-3+3), {F7 → 0}
⇒ skip, {F7 → 0, y → 0}'
}
