load ../../harness

@test "97f9ddb59a78" {
  check 'y  := NE    ;B    :=  z    *    z    ' '⇒ skip; B := (z*z), {y → 0}
⇒ B := (z*z), {y → 0}
⇒ skip, {B → 0, y → 0}'
}
