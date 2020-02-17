load ../../harness

@test "b60067cc172b" {
  check 'if (p    + z  =3 -    -1)      then 
  skip else 
z:=  -4     *  1 ' '⇒ z := (-4*1), {}
⇒ skip, {z → -4}'
}
