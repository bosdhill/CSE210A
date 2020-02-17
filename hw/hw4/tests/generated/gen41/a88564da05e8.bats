load ../../harness

@test "a88564da05e8" {
  check 'z:=    x+  -3;
z    := lB-    -4 ' '⇒ skip; z := (lB--4), {z → -3}
⇒ z := (lB--4), {z → -3}
⇒ skip, {z → 4}'
}
