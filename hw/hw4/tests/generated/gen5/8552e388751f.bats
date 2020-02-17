load ../../harness

@test "8552e388751f" {
  check 'y   :=-4     *y ;
 x    :=   y -  4    ' '⇒ skip; x := (y-4), {y → 0}
⇒ x := (y-4), {y → 0}
⇒ skip, {x → -4, y → 0}'
}
