load ../../harness

@test "06e6baeef4b0" {
  check 'if (false∨     false)      then 
F2:=1     * Yo else 
x    := 2*     3 ' '⇒ x := (2*3), {}
⇒ skip, {x → 6}'
}
