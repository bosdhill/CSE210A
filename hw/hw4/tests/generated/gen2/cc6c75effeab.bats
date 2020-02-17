load ../../harness

@test "cc6c75effeab" {
  check 'while (¬(y-    z     <4-   x4)) do skip     ' '⇒ skip, {}'
}
