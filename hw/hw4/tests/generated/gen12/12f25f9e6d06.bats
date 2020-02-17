load ../../harness

@test "12f25f9e6d06" {
  check 'if (x     -z     =  y-   y   ∨ true)     then     cU  := -4     +    x else skip ' '⇒ cU := (-4+x), {}
⇒ skip, {cU → -4}'
}
