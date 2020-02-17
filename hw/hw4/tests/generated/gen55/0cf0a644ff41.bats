load ../../harness

@test "0cf0a644ff41" {
  check 'if (Z5 *  z =   x+     4  ∧     x*     -2    <3 +     0) then  skip      else 
x:= x    *  1' '⇒ x := (x*1), {}
⇒ skip, {x → 0}'
}
