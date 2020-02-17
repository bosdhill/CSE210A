load ../../harness

@test "d8b64dc4feec" {
  check 'skip ;x :=  B     *     z   ' '⇒ x := (B*z), {}
⇒ skip, {x → 0}'
}
