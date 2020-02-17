load ../../harness

@test "5a7b3a339236" {
  check 'y    :=x   -z    ;   y:=  y  *T    ' '⇒ skip; y := (y*T), {y → 0}
⇒ y := (y*T), {y → 0}
⇒ skip, {y → 0}'
}
