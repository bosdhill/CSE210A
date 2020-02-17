load ../../harness

@test "919f764315a9" {
  check 'if (¬(1    <3    +     3))    then   z  := x     *   z     else 

z:=     -4     * z    ' '⇒ z := (-4*z), {}
⇒ skip, {z → 0}'
}
