load ../../harness

@test "6fe7b69642d8" {
  check 'y    :=    kB *    -1    ;
 y:=     0 -  -2' '⇒ skip; y := (0--2), {y → 0}
⇒ y := (0--2), {y → 0}
⇒ skip, {y → 2}'
}
