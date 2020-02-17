load ../../harness

@test "e8491c682faf" {
  check 'x :=3+    mY ' '⇒ skip, {x → 3}'
}
