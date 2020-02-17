load ../../harness

@test "a7c7493fe9bc" {
  check 'y   :=    z+     x;
  skip  ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
