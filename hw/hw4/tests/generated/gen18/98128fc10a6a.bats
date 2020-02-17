load ../../harness

@test "98128fc10a6a" {
  check 'if (x  +-3 < -3  + 2 ∨-3     +     x   =   -2    +-4)      then y     :=  0   -  0      else  z:= eA  ' '⇒ y := (0-0), {}
⇒ skip, {y → 0}'
}
