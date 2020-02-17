load ../../harness

@test "7fd34a0352f9" {
  check 'if (x- -1     <     lL    ∨  false)    then skip  else  z:=  3 +  y   ' '⇒ z := (3+y), {}
⇒ skip, {z → 3}'
}
