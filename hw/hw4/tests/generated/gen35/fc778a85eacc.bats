load ../../harness

@test "fc778a85eacc" {
  check 'if (¬(o     -   z   <3+   3)) then 
z   :=   -2  -   x     else  z   :=     0     *     x    ' '⇒ z := (0*x), {}
⇒ skip, {z → 0}'
}
