load ../../harness

@test "b2cfce438e0b" {
  check 'x   := z     -     z;
 skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
