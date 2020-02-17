load ../../harness

@test "dd389b7fa19d" {
  check 'if (z   +   z     =     x   *x∨   0     *  x=    x   * -3)     then   y    :=   0   +z      else 
y :=     2  -S' '⇒ y := (0+z), {}
⇒ skip, {y → 0}'
}
