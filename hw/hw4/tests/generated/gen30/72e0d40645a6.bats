load ../../harness

@test "72e0d40645a6" {
  check 'y   := 2 *  z   ; 
skip' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
