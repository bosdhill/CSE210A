load ../../harness

@test "4112b5bf2a71" {
  check 'n4    :=   x  *   -2    ;
z:=    x -y   ' '⇒ skip; z := (x-y), {n4 → 0}
⇒ z := (x-y), {n4 → 0}
⇒ skip, {n4 → 0, z → 0}'
}
