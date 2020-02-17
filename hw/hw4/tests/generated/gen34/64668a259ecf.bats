load ../../harness

@test "64668a259ecf" {
  check 'z   :=   x     *   z  ;
y   :=     z  -     pk    ' '⇒ skip; y := (z-pk), {z → 0}
⇒ y := (z-pk), {z → 0}
⇒ skip, {y → 0, z → 0}'
}
