load ../../harness

@test "806b37f6208f" {
  check 'g:=z  *  y   ;
  
z:= 0' '⇒ skip; z := 0, {g → 0}
⇒ z := 0, {g → 0}
⇒ skip, {g → 0, z → 0}'
}
