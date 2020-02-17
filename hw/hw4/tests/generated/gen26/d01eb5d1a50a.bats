load ../../harness

@test "d01eb5d1a50a" {
  check 'y   :=-3 - z     ;    y  :=     z+x ' '⇒ skip; y := (z+x), {y → -3}
⇒ y := (z+x), {y → -3}
⇒ skip, {y → 0}'
}
