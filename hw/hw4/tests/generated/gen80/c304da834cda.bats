load ../../harness

@test "c304da834cda" {
  check 'z    :=x  *     -3  ;
   
skip' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
