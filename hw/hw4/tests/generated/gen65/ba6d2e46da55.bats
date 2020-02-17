load ../../harness

@test "ba6d2e46da55" {
  check 'if (y     - z=     -2     +    z∧ false)    then 

   skip      else    z:= -1  *   -2' '⇒ z := (-1*-2), {}
⇒ skip, {z → 2}'
}
