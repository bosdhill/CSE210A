load ../../harness

@test "29cdb22e72f2" {
  check 'y    :=   1     *     y   ;
    y :=  -4     ' '⇒ skip; y := -4, {y → 0}
⇒ y := -4, {y → 0}
⇒ skip, {y → -4}'
}
