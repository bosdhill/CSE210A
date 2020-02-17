load ../../harness

@test "904c91470dd2" {
  check 'if (r   -   y  < x  -     -1 ∧  -2   -  -4    <0   -     4)    then skip  else  I     :=  iH   *  y    ' '⇒ I := (iH*y), {}
⇒ skip, {I → 0}'
}
