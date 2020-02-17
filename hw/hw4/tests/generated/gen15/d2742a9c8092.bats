load ../../harness

@test "d2742a9c8092" {
  check 'if (2     *x    < x    ∨     false)  then  


B    :=x *   FJ     else   v   :=     y   * -1' '⇒ v := (y*-1), {}
⇒ skip, {v → 0}'
}
