load ../../harness

@test "75c21311f54a" {
  check 'y:=3   ;   z   :=  y *  z    ' '⇒ skip; z := (y*z), {y → 3}
⇒ z := (y*z), {y → 3}
⇒ skip, {y → 3, z → 0}'
}
