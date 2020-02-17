load ../../harness

@test "b46733431452" {
  check 'y :=  x  +     -2     ;skip' '⇒ skip; skip, {y → -2}
⇒ skip, {y → -2}'
}
