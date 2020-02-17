load ../../harness

@test "b1416f2957c1" {
  check 'x := 4   -  R;
z   :=  -2   *  y' '⇒ skip; z := (-2*y), {x → 4}
⇒ z := (-2*y), {x → 4}
⇒ skip, {x → 4, z → 0}'
}
