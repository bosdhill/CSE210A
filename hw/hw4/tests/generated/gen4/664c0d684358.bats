load ../../harness

@test "664c0d684358" {
  check 'y:= 4    *  x  ;   skip' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
