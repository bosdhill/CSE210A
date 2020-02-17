load ../../harness

@test "d5f1454eb965" {
  check 'y  :=-1     *  -1   ;
 y     :=     -4     *y     ' '⇒ skip; y := (-4*y), {y → 1}
⇒ y := (-4*y), {y → 1}
⇒ skip, {y → -4}'
}
