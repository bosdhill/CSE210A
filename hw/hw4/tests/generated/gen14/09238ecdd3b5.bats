load ../../harness

@test "09238ecdd3b5" {
  check 'if (true  ∨ true)     then  x     :=    x  -     1 else   y:=  -2 *y    ' '⇒ x := (x-1), {}
⇒ skip, {x → -1}'
}
