load ../../harness

@test "6ef932ffac7c" {
  check 'if (false  ∧    z * -2=     y+   y∨ 4+     y    =  0 -4)     then 
 z :=4   -    0 else y   := 2 +   y  ' '⇒ y := (2+y), {}
⇒ skip, {y → 2}'
}
