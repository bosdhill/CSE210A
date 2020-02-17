load ../../harness

@test "3b0f59d1a427" {
  check 'while (¬true)     do x  :=     -4+ cD     ' '⇒ skip, {}'
}
