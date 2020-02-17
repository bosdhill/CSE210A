load ../../harness

@test "954d7874f28a" {
  check 'if (true    ∨    y    +    3  = y     -     x)  then 
    z   :=     -2 *   4  else  skip     ' '⇒ z := (-2*4), {}
⇒ skip, {z → -8}'
}
