load ../../harness

@test "1dcd5e6fcc22" {
  check 'x  :=1  -     4    ; 
z   :=0  +  y ' '⇒ skip; z := (0+y), {x → -3}
⇒ z := (0+y), {x → -3}
⇒ skip, {x → -3, z → 0}'
}
