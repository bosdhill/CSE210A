load ../../harness

@test "8b1116b33c08" {
  check 'if (y  +-3     =2 *   3∧    H +  KT   <r* x)      then  
 z    :=y    *    (0   +    2)      else   y :=   x     -    x ' '⇒ y := (x-x), {}
⇒ skip, {y → 0}'
}
