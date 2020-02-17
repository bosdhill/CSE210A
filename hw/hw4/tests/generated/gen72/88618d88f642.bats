load ../../harness

@test "88618d88f642" {
  check 'y :=-2   +  1  ; z :=x   *   3     ' '⇒ skip; z := (x*3), {y → -1}
⇒ z := (x*3), {y → -1}
⇒ skip, {y → -1, z → 0}'
}
