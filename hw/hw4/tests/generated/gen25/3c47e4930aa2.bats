load ../../harness

@test "3c47e4930aa2" {
  check 'y:=   y  +    zs     ;
z    :=  x     -     1    ' '⇒ skip; z := (x-1), {y → 0}
⇒ z := (x-1), {y → 0}
⇒ skip, {y → 0, z → -1}'
}
