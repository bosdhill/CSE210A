load ../../harness

@test "8492294cbdf1" {
  check 'y  :=   -4   - DD    ;

z   := -1     ' '⇒ skip; z := -1, {y → -4}
⇒ z := -1, {y → -4}
⇒ skip, {y → -4, z → -1}'
}
