load ../../harness

@test "a3f44cb87f2d" {
  check 'if (¬(4  -    z =   p    + 2+     x   ∨  false))      then  y :=z    -     4    else skip' '⇒ y := (z-4), {}
⇒ skip, {y → -4}'
}
