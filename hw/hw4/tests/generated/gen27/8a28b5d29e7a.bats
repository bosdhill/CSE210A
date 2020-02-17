load ../../harness

@test "8a28b5d29e7a" {
  check 'y   :=   x +     1;    
skip' '⇒ skip; skip, {y → 1}
⇒ skip, {y → 1}'
}
