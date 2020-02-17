load ../../harness

@test "4324782e0cc8" {
  check 'x    :=     -2    -z     ;  
z :=   y     +     4 ' '⇒ skip; z := (y+4), {x → -2}
⇒ z := (y+4), {x → -2}
⇒ skip, {x → -2, z → 4}'
}
