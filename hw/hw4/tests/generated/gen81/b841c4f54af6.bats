load ../../harness

@test "b841c4f54af6" {
  check 'if (0+  0 = 4  *   -2 ∨   -3     *x     <     z *    n)    then ME  :=   x +  0  else x   :=   x ' '⇒ x := x, {}
⇒ skip, {x → 0}'
}
