load ../../harness

@test "31b793ae31e1" {
  check 'if (-3     +     -3=    z *-1     ∧   z+     y    =     0 * z)   then 

  F:=   -3     +     z      else  
x  :=   y     +    z' '⇒ x := (y+z), {}
⇒ skip, {x → 0}'
}
