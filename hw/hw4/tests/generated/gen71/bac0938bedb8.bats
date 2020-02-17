load ../../harness

@test "bac0938bedb8" {
  check 'y   :=     z   +    x' '⇒ skip, {y → 0}'
}
