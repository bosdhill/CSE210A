load ../../harness

@test "cb90779f7a98" {
  check 'if (-1    +     y =   x   *    2∨     false)      then y  :=   z  *     0    else x :=  x    *  z    ' '⇒ x := (x*z), {}
⇒ skip, {x → 0}'
}
