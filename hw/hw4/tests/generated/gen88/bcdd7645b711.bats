load ../../harness

@test "bcdd7645b711" {
  check 'if (3*   3   =   1     +   2 ∧ -1    *z =     -4-     z)      then skip   else    x:=  -3    -     r    ' '⇒ x := (-3-r), {}
⇒ skip, {x → -3}'
}
