load ../../harness

@test "57f4cffbec11" {
  check 'if (iu  --3<   -1  +   0  ∨  y    -y    =    4    +  z)  then   
skip else 
y:= 3-  1   ' '⇒ y := (3-1), {}
⇒ skip, {y → 2}'
}
