load ../../harness

@test "bf0403a31348" {
  check 'u :=    z  -    -4   ;


u    :=     y    +   4' '⇒ skip; u := (y+4), {u → 4}
⇒ u := (y+4), {u → 4}
⇒ skip, {u → 4}'
}
