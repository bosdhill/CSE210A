load ../../harness

@test "e06508f7fd8f" {
  check 'R    :=    y  *    1  ;
y :=  4   *     y   ' '⇒ skip; y := (4*y), {R → 0}
⇒ y := (4*y), {R → 0}
⇒ skip, {R → 0, y → 0}'
}
