load ../../harness

@test "aa48b9cac937" {
  check 'm7 :=  z +3    ; 
y  := (x   -x)  *  0 ' '⇒ skip; y := ((x-x)*0), {m7 → 3}
⇒ y := ((x-x)*0), {m7 → 3}
⇒ skip, {m7 → 3, y → 0}'
}
