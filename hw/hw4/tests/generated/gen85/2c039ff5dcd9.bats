load ../../harness

@test "2c039ff5dcd9" {
  check 'x  := 1    +   -1 ;N1     :=x  +4  ' '⇒ skip; N1 := (x+4), {x → 0}
⇒ N1 := (x+4), {x → 0}
⇒ skip, {N1 → 4, x → 0}'
}
