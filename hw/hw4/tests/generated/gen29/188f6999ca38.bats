load ../../harness

@test "188f6999ca38" {
  check 'k6:=   y   -G9   ' '⇒ skip, {k6 → 0}'
}
