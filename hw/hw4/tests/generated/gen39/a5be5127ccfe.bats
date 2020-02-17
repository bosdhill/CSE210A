load ../../harness

@test "a5be5127ccfe" {
  check 'z     :=     4  +  -2  ;  y   :=     1  +x' '⇒ skip; y := (1+x), {z → 2}
⇒ y := (1+x), {z → 2}
⇒ skip, {y → 1, z → 2}'
}
