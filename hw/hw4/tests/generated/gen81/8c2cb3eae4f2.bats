load ../../harness

@test "8c2cb3eae4f2" {
  check 'if (true ∨  -2    +   x     =   -3)      then x:=  y -z    else y    := -1    - 2  ' '⇒ x := (y-z), {}
⇒ skip, {x → 0}'
}
