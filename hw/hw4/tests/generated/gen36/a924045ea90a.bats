load ../../harness

@test "a924045ea90a" {
  check 'if (z     +-4   =-2∧     s7  *     x    <  x)      then x  := 3  +z     else x     :=    2 ' '⇒ x := 2, {}
⇒ skip, {x → 2}'
}
