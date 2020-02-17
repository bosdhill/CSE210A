load ../../harness

@test "63115fd1243b" {
  check 'y    := 4    +x    ; 
 M     := y   *    -4' '⇒ skip; M := (y*-4), {y → 4}
⇒ M := (y*-4), {y → 4}
⇒ skip, {M → -16, y → 4}'
}
