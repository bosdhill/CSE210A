load ../../harness

@test "53b9b1caec90" {
  check 'z  :=   z -   z;  
 skip' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
