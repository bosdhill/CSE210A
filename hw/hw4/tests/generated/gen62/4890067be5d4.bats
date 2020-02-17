load ../../harness

@test "4890067be5d4" {
  check 'a   :=  3 -   Uj ;
  skip     ' '⇒ skip; skip, {a → 3}
⇒ skip, {a → 3}'
}
