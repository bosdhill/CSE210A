load ../../harness

@test "967cd3dae0d4" {
  check 'y :=    -3*W+ x;
y     :=x     *     y ' '⇒ skip; y := (x*y), {y → 0}
⇒ y := (x*y), {y → 0}
⇒ skip, {y → 0}'
}
