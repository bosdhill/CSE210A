load ../../harness

@test "52d972f7504c" {
  check 'x:=4  *     z    ;  y :=   -3     ' '⇒ skip; y := -3, {x → 0}
⇒ y := -3, {x → 0}
⇒ skip, {x → 0, y → -3}'
}
