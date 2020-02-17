load ../../harness

@test "7c53c8d0e764" {
  check 'if (-4     -    -2     <z-     y     ∨1   =y-   y)     then x :=d6   +  x else  skip     ' '⇒ x := (d6+x), {}
⇒ skip, {x → 0}'
}
