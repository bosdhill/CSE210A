load ../../harness

@test "22019f942672" {
  check 'n :=   -3    +  -4   ;y  :=    -4     *  4 ' '⇒ skip; y := (-4*4), {n → -7}
⇒ y := (-4*4), {n → -7}
⇒ skip, {n → -7, y → -16}'
}
