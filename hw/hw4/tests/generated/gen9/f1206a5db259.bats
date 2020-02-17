load ../../harness

@test "f1206a5db259" {
  check 'y:=   0 ;
 i3   :=  z    +  -1     ' '⇒ skip; i3 := (z+-1), {y → 0}
⇒ i3 := (z+-1), {y → 0}
⇒ skip, {i3 → -1, y → 0}'
}
