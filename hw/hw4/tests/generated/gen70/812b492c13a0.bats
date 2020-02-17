load ../../harness

@test "812b492c13a0" {
  check 'if (-3    * x   =    y   -x ∨  x    *   y     <   0 -   x) then skip    else y :=  2    +x ' '⇒ skip, {}'
}
