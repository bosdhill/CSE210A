load ../../harness

@test "9e2b22b5f748" {
  check 'x   :=  -3* -1;
 skip' '⇒ skip; skip, {x → 3}
⇒ skip, {x → 3}'
}
