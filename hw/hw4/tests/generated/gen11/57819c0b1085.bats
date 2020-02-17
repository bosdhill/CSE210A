load ../../harness

@test "57819c0b1085" {
  check 'if (true∨   y  *     0 <     0 + x)   then 
D    :=   Yn + z else U  :=     -4  +   4    ' '⇒ D := (Yn+z), {}
⇒ skip, {D → 0}'
}
