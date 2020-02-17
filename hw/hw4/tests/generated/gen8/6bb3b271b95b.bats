load ../../harness

@test "6bb3b271b95b" {
  check 'z:=     x -  x    ;C    :=y+    y' '⇒ skip; C := (y+y), {z → 0}
⇒ C := (y+y), {z → 0}
⇒ skip, {C → 0, z → 0}'
}
