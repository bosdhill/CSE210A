load ../../harness

@test "d9daf569a98e" {
  check 'lJ  :=   3     -    z;x := x - x  ' '⇒ skip; x := (x-x), {lJ → 3}
⇒ x := (x-x), {lJ → 3}
⇒ skip, {lJ → 3, x → 0}'
}
