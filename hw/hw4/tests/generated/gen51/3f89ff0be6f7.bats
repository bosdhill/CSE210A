load ../../harness

@test "3f89ff0be6f7" {
  check 'Ve :=   -2   -  -3   ;
z   :=   3  *     -4    ' '⇒ skip; z := (3*-4), {Ve → 1}
⇒ z := (3*-4), {Ve → 1}
⇒ skip, {Ve → 1, z → -12}'
}
