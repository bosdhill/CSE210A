load ../../harness

@test "5bafee007666" {
  check 'y :=0+ 1    ;
z :=z*z  ' '⇒ skip; z := (z*z), {y → 1}
⇒ z := (z*z), {y → 1}
⇒ skip, {y → 1, z → 0}'
}
