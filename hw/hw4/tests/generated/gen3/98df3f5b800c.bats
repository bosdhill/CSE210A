load ../../harness

@test "98df3f5b800c" {
  check 'if (z *  z<  MZ     +    2     ∨     x *     f2= 2    - 2)      then 
 y    := -4   *    -4   else   y    :=    0   - x ' '⇒ y := (-4*-4), {}
⇒ skip, {y → 16}'
}
