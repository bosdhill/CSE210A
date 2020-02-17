load ../../harness

@test "dea2c1e5ad03" {
  check 'x   :=     z *    x;
skip  ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
