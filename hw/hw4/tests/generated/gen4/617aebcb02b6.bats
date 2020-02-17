load ../../harness

@test "617aebcb02b6" {
  check 'if (false ∨z    +     4    =    1   +     -3) then   skip    else y :=   4+     3  ' '⇒ y := (4+3), {}
⇒ skip, {y → 7}'
}
