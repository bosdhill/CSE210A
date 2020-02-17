load ../../harness

@test "3d9a539022b6" {
  check 'x :=l -   x;

 y    :=0   +    z     ' '⇒ skip; y := (0+z), {x → 0}
⇒ y := (0+z), {x → 0}
⇒ skip, {x → 0, y → 0}'
}
