load ../../harness

@test "b225ce22e7cb" {
  check 'RZ :=  -3     *   h ;
y   :=   z     *  x  ' '⇒ skip; y := (z*x), {RZ → 0}
⇒ y := (z*x), {RZ → 0}
⇒ skip, {RZ → 0, y → 0}'
}
