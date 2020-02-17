load ../../harness

@test "d779b80d53f0" {
  check 'if (¬true) then 
  
skip  else  y   :=Y * y   ' '⇒ y := (Y*y), {}
⇒ skip, {y → 0}'
}
