load ../../harness

@test "9938e65aee17" {
  check 'y:=     Q9*     y   ;    
v   :=  z+   3   ' '⇒ skip; v := (z+3), {y → 0}
⇒ v := (z+3), {y → 0}
⇒ skip, {v → 3, y → 0}'
}
