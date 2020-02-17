load ../../harness

@test "0c334ca2bfb0" {
  check 'x:=  z    *    L     ; 

y    := x -     -4    ' '⇒ skip; y := (x--4), {x → 0}
⇒ y := (x--4), {x → 0}
⇒ skip, {x → 0, y → 4}'
}
