load ../../harness

@test "0a79b43852cd" {
  check 'G0  := z  -  4   ; 
x   := x   +  y     ' '⇒ skip; x := (x+y), {G0 → -4}
⇒ x := (x+y), {G0 → -4}
⇒ skip, {G0 → -4, x → 0}'
}
