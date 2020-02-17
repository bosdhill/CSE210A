load ../../harness

@test "ce613fbd28ae" {
  check 'z   :=  1  -    -1    ;

z     :=     y   -    -2    ' '⇒ skip; z := (y--2), {z → 2}
⇒ z := (y--2), {z → 2}
⇒ skip, {z → 2}'
}
