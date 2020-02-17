load ../../harness

@test "83156c210217" {
  check 'x  :=-1     *     -2;

 skip' '⇒ skip; skip, {x → 2}
⇒ skip, {x → 2}'
}
