load ../../harness

@test "abb508e96832" {
  check 'if (3+   -4  =   y∧     s0  +k  = I9     +     y)      then skip    else x     := 0-   x' '⇒ x := (0-x), {}
⇒ skip, {x → 0}'
}
