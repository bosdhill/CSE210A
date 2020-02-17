load ../../harness

@test "cc327d8feaa0" {
  check 'z   :=    1 +     z  ; 

  z:=     x   - x    ' '⇒ skip; z := (x-x), {z → 1}
⇒ z := (x-x), {z → 1}
⇒ skip, {z → 0}'
}
