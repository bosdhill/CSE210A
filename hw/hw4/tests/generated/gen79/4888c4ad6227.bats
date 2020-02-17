load ../../harness

@test "4888c4ad6227" {
  check 'if (x  +     x   <     4     +    z  ∧false) then 
  skip  else   z:=  1 -   z ' '⇒ z := (1-z), {}
⇒ skip, {z → 1}'
}
