load ../../harness

@test "5fc525b2efd2" {
  check 'if (¬((4 +     y9)   -   z    < 4+    0))  then 
  z     :=   4  else skip     ' '⇒ z := 4, {}
⇒ skip, {z → 4}'
}
