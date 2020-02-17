load ../../harness

@test "419daaa894fc" {
  check 'if (y    +   1 < x  +     y)      then 
z :=    x-  y      else x:=    z     +  1    ' '⇒ x := (z+1), {}
⇒ skip, {x → 1}'
}
