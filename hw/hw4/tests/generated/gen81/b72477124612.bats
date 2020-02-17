load ../../harness

@test "b72477124612" {
  check 'y:= w    *   4  ;
skip     ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
