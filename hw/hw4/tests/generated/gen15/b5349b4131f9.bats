load ../../harness

@test "b5349b4131f9" {
  check 'z  :=    0    -z  ;

z    :=4+   -4   ' '⇒ skip; z := (4+-4), {z → 0}
⇒ z := (4+-4), {z → 0}
⇒ skip, {z → 0}'
}
