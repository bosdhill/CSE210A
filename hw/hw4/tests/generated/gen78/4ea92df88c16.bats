load ../../harness

@test "4ea92df88c16" {
  check 'x  :=   x     +    3  ; 


y:=  -1-z' '⇒ skip; y := (-1-z), {x → 3}
⇒ y := (-1-z), {x → 3}
⇒ skip, {x → 3, y → -1}'
}
