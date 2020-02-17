load ../../harness

@test "de63d4291923" {
  check 'while 4+    y<x     *   3∧  x+    y    <   1*     x     do skip     ' '⇒ skip, {}'
}
