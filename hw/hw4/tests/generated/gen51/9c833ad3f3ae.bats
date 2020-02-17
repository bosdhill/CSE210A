load ../../harness

@test "9c833ad3f3ae" {
  check 'dS:= y  +    z   ; 
 I :=    y     +    z    ' '⇒ skip; I := (y+z), {dS → 0}
⇒ I := (y+z), {dS → 0}
⇒ skip, {I → 0, dS → 0}'
}
