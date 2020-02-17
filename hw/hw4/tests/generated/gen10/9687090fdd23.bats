load ../../harness

@test "9687090fdd23" {
  check 'y:=  yi     * y   ;y  := -1 + x  ' '⇒ skip; y := (-1+x), {y → 0}
⇒ y := (-1+x), {y → 0}
⇒ skip, {y → -1}'
}
