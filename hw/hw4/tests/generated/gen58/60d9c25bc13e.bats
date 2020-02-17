load ../../harness

@test "60d9c25bc13e" {
  check 'if (true  ∧     3     *  2  <    -1  +    x)      then 

 skip      else y:=     x  +    y ' '⇒ y := (x+y), {}
⇒ skip, {y → 0}'
}
