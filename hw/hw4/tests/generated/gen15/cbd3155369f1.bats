load ../../harness

@test "cbd3155369f1" {
  check 'y  := z   *z    ;

x:= 2     *4  ' '⇒ skip; x := (2*4), {y → 0}
⇒ x := (2*4), {y → 0}
⇒ skip, {x → 8, y → 0}'
}
