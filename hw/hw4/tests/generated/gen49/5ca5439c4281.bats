load ../../harness

@test "5ca5439c4281" {
  check 'x     :=   x +     -4   ;
S  := z- x    ' '⇒ skip; S := (z-x), {x → -4}
⇒ S := (z-x), {x → -4}
⇒ skip, {S → 4, x → -4}'
}
