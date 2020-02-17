load ../../harness

@test "dda52d992331" {
  check 'if true    then  z   :=     y  *z    else y     :=     y*  y    ' '⇒ z := (y*z), {}
⇒ skip, {z → 0}'
}
