load ../../harness

@test "60f5c4899e6b" {
  check 'y   :=x  ;
 z :=   z  +   y     ' '⇒ skip; z := (z+y), {y → 0}
⇒ z := (z+y), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
