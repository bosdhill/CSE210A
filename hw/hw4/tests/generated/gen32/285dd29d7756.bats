load ../../harness

@test "285dd29d7756" {
  check 'if (false     ∨  -4= y   *   x)    then skip else  

y     :=0 +x    +    x' '⇒ y := ((0+x)+x), {}
⇒ skip, {y → 0}'
}
