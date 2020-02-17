load ../../harness

@test "c030eae1a328" {
  check 'if (z+    y     <y    +-1∧   -4     *2=y   * 4)     then z  :=  y     +     0 else  z:=4 - 0' '⇒ z := (4-0), {}
⇒ skip, {z → 4}'
}
