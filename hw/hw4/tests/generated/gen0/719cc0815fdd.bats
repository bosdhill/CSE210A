load ../../harness

@test "719cc0815fdd" {
  check 'if (¬(Iq  +   x   =  z     +   L))    then      z:=     -2     +    y     else   y     :=     -2     -     y   ' '⇒ y := (-2-y), {}
⇒ skip, {y → -2}'
}
