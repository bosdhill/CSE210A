load ../../harness

@test "16678584bbc1" {
  check 'if (¬(1 -     -2  <  x     *    2)) then  

y  := z     +    V6     else   
  skip' '⇒ y := (z+V6), {}
⇒ skip, {y → 0}'
}
