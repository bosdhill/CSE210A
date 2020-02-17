load ../../harness

@test "e3d28fda9914" {
  check 'if (¬(y    -   O<     1  +     y))     then     x    :=   -4     *    2   else c   :=     1+    -3    ' '⇒ c := (1+-3), {}
⇒ skip, {c → -2}'
}
