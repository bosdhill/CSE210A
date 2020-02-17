load ../../harness

@test "bca02b03cfde" {
  check 'x     :=  -2   +    x   ; 
 
y :=    x  -  x ' '⇒ skip; y := (x-x), {x → -2}
⇒ y := (x-x), {x → -2}
⇒ skip, {x → -2, y → 0}'
}
