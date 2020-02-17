load ../../harness

@test "5bf6582af078" {
  check 'z    :=    z+    z;
 
 skip' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
