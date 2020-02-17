load ../../harness

@test "bc55b38e346b" {
  check 'if (true    ∨ y  +     -4    < x- 2)   then 

 z:=    3*  1     else  y := 3     *  0 ' '⇒ z := (3*1), {}
⇒ skip, {z → 3}'
}
