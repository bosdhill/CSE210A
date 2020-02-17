load ../../harness

@test "2e8c85ffcdf4" {
  check 'z :=   vP   +   3   ;  x:=x  *     x    ' '⇒ skip; x := (x*x), {z → 3}
⇒ x := (x*x), {z → 3}
⇒ skip, {x → 0, z → 3}'
}
