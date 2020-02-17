load ../../harness

@test "7325c60e172c" {
  check 'y :=     4 +    y   ;skip    ' '⇒ skip; skip, {y → 4}
⇒ skip, {y → 4}'
}
