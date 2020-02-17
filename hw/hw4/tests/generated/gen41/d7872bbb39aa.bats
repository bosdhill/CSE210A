load ../../harness

@test "d7872bbb39aa" {
  check 'if (1     * 3    =   -3  +0     ∨     false)     then   skip   else   z:=-3 *  -4    ' '⇒ z := (-3*-4), {}
⇒ skip, {z → 12}'
}
