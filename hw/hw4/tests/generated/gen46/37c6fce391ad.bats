load ../../harness

@test "37c6fce391ad" {
  check 'x:=   0    +  z    ; 
R  :=     XY     ' '⇒ skip; R := XY, {x → 0}
⇒ R := XY, {x → 0}
⇒ skip, {R → 0, x → 0}'
}
