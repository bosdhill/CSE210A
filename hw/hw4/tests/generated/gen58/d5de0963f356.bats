load ../../harness

@test "d5de0963f356" {
  check 'y :=  3   -     F;

x :=   x    -4' '⇒ skip; x := (x-4), {y → 3}
⇒ x := (x-4), {y → 3}
⇒ skip, {x → -4, y → 3}'
}
