load ../../harness

@test "dfd23f1e824a" {
  check 'if (false    ∨     4    <   O2 -   -2)    then   
y     :=    y     -   y  else  y    :=     x   *  -2    ' '⇒ y := (x*-2), {}
⇒ skip, {y → 0}'
}
