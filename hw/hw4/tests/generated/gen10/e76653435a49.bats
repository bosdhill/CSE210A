load ../../harness

@test "e76653435a49" {
  check 'tX:=-3*  0  ;
z     :=   x  +-4  + 4 ' '⇒ skip; z := ((x+-4)+4), {tX → 0}
⇒ z := ((x+-4)+4), {tX → 0}
⇒ skip, {tX → 0, z → 0}'
}
