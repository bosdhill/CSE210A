load ../../harness

@test "17ed42eab198" {
  check 'o   :=     4  *    1 ;y    :=     -1  *z ' '⇒ skip; y := (-1*z), {o → 4}
⇒ y := (-1*z), {o → 4}
⇒ skip, {o → 4, y → 0}'
}
