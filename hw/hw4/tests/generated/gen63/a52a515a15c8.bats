load ../../harness

@test "a52a515a15c8" {
  check 'if (true   ∧ Fz*     x     <0*-3)  then 
x    :=     -3  -y    else   V2   :=   3     +     z     ' '⇒ V2 := (3+z), {}
⇒ skip, {V2 → 3}'
}
