load ../../harness

@test "4dbb9540fc93" {
  check 'while 2  -    x    =3+   -4     ∨  false      do x  :=   y    *   -3' '⇒ skip, {}'
}
