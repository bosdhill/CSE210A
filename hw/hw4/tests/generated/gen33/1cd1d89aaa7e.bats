load ../../harness

@test "1cd1d89aaa7e" {
  check 'x  :=    3     -  z;
x  := x -   3  ' '⇒ skip; x := (x-3), {x → 3}
⇒ x := (x-3), {x → 3}
⇒ skip, {x → 0}'
}
