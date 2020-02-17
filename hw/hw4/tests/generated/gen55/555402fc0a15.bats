load ../../harness

@test "555402fc0a15" {
  check 'GX  :=   4    - x;
 PZ   :=    Rk *  x' '⇒ skip; PZ := (Rk*x), {GX → 4}
⇒ PZ := (Rk*x), {GX → 4}
⇒ skip, {GX → 4, PZ → 0}'
}
