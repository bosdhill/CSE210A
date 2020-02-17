load ../../harness

@test "68229a305c59" {
  check 'if (true    ∧     Q -     x    <    2)   then  

 y := 1 *    z     else 

y    := y - z' '⇒ y := (1*z), {}
⇒ skip, {y → 0}'
}
