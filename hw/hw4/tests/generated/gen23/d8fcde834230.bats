load ../../harness

@test "d8fcde834230" {
  check 'if (y     +-1     < y  +     0    ∨-3    *    z    <     -2   +   -3)  then y  := x *     KM      else  y    :=    1* G    ' '⇒ y := (x*KM), {}
⇒ skip, {y → 0}'
}
