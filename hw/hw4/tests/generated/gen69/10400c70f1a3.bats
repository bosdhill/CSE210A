load ../../harness

@test "10400c70f1a3" {
  check 'if (3 -    z=  4     *   l    ∨   false)     then  

skip      else z     :=  4 *     x ' '⇒ z := (4*x), {}
⇒ skip, {z → 0}'
}
