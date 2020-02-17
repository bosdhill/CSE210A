load ../../harness

@test "d3e75262684c" {
  check 'y :=    4  *   y   ;skip   ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
