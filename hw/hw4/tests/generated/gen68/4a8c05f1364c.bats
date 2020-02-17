load ../../harness

@test "4a8c05f1364c" {
  check 'z :=  0;
x :=y --1  ' '⇒ skip; x := (y--1), {z → 0}
⇒ x := (y--1), {z → 0}
⇒ skip, {x → 1, z → 0}'
}
