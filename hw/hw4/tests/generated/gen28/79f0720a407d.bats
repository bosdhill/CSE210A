load ../../harness

@test "79f0720a407d" {
  check 'if (y  +   -2<   x     *    3   ∨   true)  then 

x :=z-  z     else 

z   :=    z    +     y     ' '⇒ x := (z-z), {}
⇒ skip, {x → 0}'
}
