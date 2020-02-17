load ../../harness

@test "ecaea83170df" {
  check 'y  :=     y   -y ;
 z   :=   x     +  -4' '⇒ skip; z := (x+-4), {y → 0}
⇒ z := (x+-4), {y → 0}
⇒ skip, {y → 0, z → -4}'
}
