load ../../harness

@test "05d0db0f6f0a" {
  check 'y   :=  z ;   

skip' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
