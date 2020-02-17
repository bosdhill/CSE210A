load ../../harness

@test "d744240e941a" {
  check 'z  :=    x- z    ; 

y:= 4    ' '⇒ skip; y := 4, {z → 0}
⇒ y := 4, {z → 0}
⇒ skip, {y → 4, z → 0}'
}
