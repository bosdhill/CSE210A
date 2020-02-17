load ../../harness

@test "e495e1b1adcd" {
  check 'z  :=1+3  ;C   :=1     - 3' '⇒ skip; C := (1-3), {z → 4}
⇒ C := (1-3), {z → 4}
⇒ skip, {C → -2, z → 4}'
}
