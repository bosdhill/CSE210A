load ../../harness

@test "76606a29f795" {
  check 'if (4   +   3    =     y    -  1)    then   
eq  := -4   -    -3 else 
z :=     z  -  z  ' '⇒ z := (z-z), {}
⇒ skip, {z → 0}'
}
