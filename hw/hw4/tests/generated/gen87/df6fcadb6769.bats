load ../../harness

@test "df6fcadb6769" {
  check 'x   :=y  *z;
 skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
