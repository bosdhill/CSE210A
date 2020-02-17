load ../../harness

@test "f05e0515aa4d" {
  check 'if (z *     1     = y+    -1    ∧     y   *    -2 <     fs    +  2)  then z  :=     x     +  x      else    x  :=4    ' '⇒ x := 4, {}
⇒ skip, {x → 4}'
}
