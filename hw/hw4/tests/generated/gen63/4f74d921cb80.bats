load ../../harness

@test "4f74d921cb80" {
  check 'if (0  + -4 = z∧     2 -  z =  -2)      then skip else y     :=   z     +     0 ' '⇒ y := (z+0), {}
⇒ skip, {y → 0}'
}
