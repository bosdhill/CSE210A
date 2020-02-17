load ../../harness

@test "76de303d43be" {
  check 'y :=  x    *  x     ;

y  := z     *  x  ' '⇒ skip; y := (z*x), {y → 0}
⇒ y := (z*x), {y → 0}
⇒ skip, {y → 0}'
}
