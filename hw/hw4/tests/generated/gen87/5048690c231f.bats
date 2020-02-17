load ../../harness

@test "5048690c231f" {
  check 'y  :=  a   -    x     ;

skip     ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
