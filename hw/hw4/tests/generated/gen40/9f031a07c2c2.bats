load ../../harness

@test "9f031a07c2c2" {
  check 'z   :=  y     -  y     ; 

y :=     3    -y ' '⇒ skip; y := (3-y), {z → 0}
⇒ y := (3-y), {z → 0}
⇒ skip, {y → 3, z → 0}'
}
