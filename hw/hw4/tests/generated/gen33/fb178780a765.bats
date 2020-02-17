load ../../harness

@test "fb178780a765" {
  check 'while false   ∧y-    3     <  z *     y do  Xc     := x+ 1  ' '⇒ skip, {}'
}
