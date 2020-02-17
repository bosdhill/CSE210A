load ../../harness

@test "c5b5fe544c2a" {
  check 'y   :=4+    -3  ;  

z   :=  1-     2   ' '⇒ skip; z := (1-2), {y → 1}
⇒ z := (1-2), {y → 1}
⇒ skip, {y → 1, z → -1}'
}
