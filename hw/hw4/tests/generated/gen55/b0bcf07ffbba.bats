load ../../harness

@test "b0bcf07ffbba" {
  check 'x  :=  z   -  x    ;y  :=-1*  4     ' '⇒ skip; y := (-1*4), {x → 0}
⇒ y := (-1*4), {x → 0}
⇒ skip, {x → 0, y → -4}'
}
