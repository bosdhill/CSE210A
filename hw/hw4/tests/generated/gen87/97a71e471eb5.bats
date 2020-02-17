load ../../harness

@test "97a71e471eb5" {
  check 'if (¬(-1   -    oF     <  -1   +   0))    then  y   :=    3  + x  else skip ' '⇒ y := (3+x), {}
⇒ skip, {y → 3}'
}
