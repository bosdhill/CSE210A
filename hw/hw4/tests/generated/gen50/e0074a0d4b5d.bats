load ../../harness

@test "e0074a0d4b5d" {
  check 'if (3   *     -3 <-1  -   z ∨   true)  then z:=     y      else 
skip   ' '⇒ z := y, {}
⇒ skip, {z → 0}'
}
