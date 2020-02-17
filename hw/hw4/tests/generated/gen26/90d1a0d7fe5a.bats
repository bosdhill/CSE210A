load ../../harness

@test "90d1a0d7fe5a" {
  check 'x  :=     y + x; 
x:=  2 +  y     ' '⇒ skip; x := (2+y), {x → 0}
⇒ x := (2+y), {x → 0}
⇒ skip, {x → 2}'
}
