load ../../harness

@test "7898b192a734" {
  check 'z   :=    x     *  1    ; z :=     lM-    x ' '⇒ skip; z := (lM-x), {z → 0}
⇒ z := (lM-x), {z → 0}
⇒ skip, {z → 0}'
}
