load ../../harness

@test "20c608219933" {
  check 'y := -1    *-1 ; z:=     3 *3 ' '⇒ skip; z := (3*3), {y → 1}
⇒ z := (3*3), {y → 1}
⇒ skip, {y → 1, z → 9}'
}
