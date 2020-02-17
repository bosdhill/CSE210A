load ../../harness

@test "90517dab923f" {
  check 'z  :=     -3- 2    ;  
y   :=     x   - y' '⇒ skip; y := (x-y), {z → -5}
⇒ y := (x-y), {z → -5}
⇒ skip, {y → 0, z → -5}'
}
