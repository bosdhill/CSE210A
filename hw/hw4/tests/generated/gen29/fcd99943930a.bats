load ../../harness

@test "fcd99943930a" {
  check 'if (y-     0  =  X   ∨false)    then w:= y    +     z   else     skip  ' '⇒ w := (y+z), {}
⇒ skip, {w → 0}'
}
