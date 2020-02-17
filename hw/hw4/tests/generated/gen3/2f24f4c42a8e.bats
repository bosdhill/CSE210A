load ../../harness

@test "2f24f4c42a8e" {
  check 'EO:=    y     +     4    ;z :=   -2 - cL     ' '⇒ skip; z := (-2-cL), {EO → 4}
⇒ z := (-2-cL), {EO → 4}
⇒ skip, {EO → 4, z → -2}'
}
