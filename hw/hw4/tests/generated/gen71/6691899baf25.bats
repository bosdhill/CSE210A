load ../../harness

@test "6691899baf25" {
  check 'a:=  y*  y     ; y :=     1     -     R   ' '⇒ skip; y := (1-R), {a → 0}
⇒ y := (1-R), {a → 0}
⇒ skip, {a → 0, y → 1}'
}
