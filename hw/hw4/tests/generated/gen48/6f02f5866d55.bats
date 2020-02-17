load ../../harness

@test "6f02f5866d55" {
  check 'y     :=  -4 *     0    ;y   :=1     + 2' '⇒ skip; y := (1+2), {y → 0}
⇒ y := (1+2), {y → 0}
⇒ skip, {y → 3}'
}
