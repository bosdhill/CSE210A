load ../../harness

@test "88c3945af26f" {
  check 'y   :=   z    -   x  ; y   := Fi-    4   ' '⇒ skip; y := (Fi-4), {y → 0}
⇒ y := (Fi-4), {y → 0}
⇒ skip, {y → -4}'
}
