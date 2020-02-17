load ../../harness

@test "6e3182a11a97" {
  check 'if (-2     -   M  =     A  +    z     ∨   3   *   -1     <   -2  * -4)    then 
y     :=    -4  +-2     else   z:=    x-     z ' '⇒ y := (-4+-2), {}
⇒ skip, {y → -6}'
}
