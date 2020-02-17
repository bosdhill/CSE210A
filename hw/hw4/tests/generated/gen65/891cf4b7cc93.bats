load ../../harness

@test "891cf4b7cc93" {
  check 'y   :=   4  *    x; y:=  3 +     gZ   ' '⇒ skip; y := (3+gZ), {y → 0}
⇒ y := (3+gZ), {y → 0}
⇒ skip, {y → 3}'
}
