load ../../harness

@test "cf161ae69464" {
  check 'x    :=    -4  -    z     ;
 
 y:=  2   *  z    ' '⇒ skip; y := (2*z), {x → -4}
⇒ y := (2*z), {x → -4}
⇒ skip, {x → -4, y → 0}'
}
