load ../../harness

@test "557b37e41002" {
  check 'y  :=-4  *     y;z    :=   y + -3' '⇒ skip; z := (y+-3), {y → 0}
⇒ z := (y+-3), {y → 0}
⇒ skip, {y → 0, z → -3}'
}
