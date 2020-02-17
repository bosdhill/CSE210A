load ../../harness

@test "3d91e8fa417f" {
  check 'if (false    ∧false)     then skip   else y:= rt    +   l ' '⇒ y := (rt+l), {}
⇒ skip, {y → 0}'
}
