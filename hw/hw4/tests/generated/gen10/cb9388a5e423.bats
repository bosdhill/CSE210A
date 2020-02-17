load ../../harness

@test "cb9388a5e423" {
  check 'y := x     *     y   ;


y:=  x   -   y    ' '⇒ skip; y := (x-y), {y → 0}
⇒ y := (x-y), {y → 0}
⇒ skip, {y → 0}'
}
