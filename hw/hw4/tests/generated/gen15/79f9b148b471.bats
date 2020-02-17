load ../../harness

@test "79f9b148b471" {
  check 'if (¬(x   +    y     <   z   -   -4))     then  
z   := -1    +    y      else d :=z     -    3' '⇒ d := (z-3), {}
⇒ skip, {d → -3}'
}
