load ../../harness

@test "615530c14bc7" {
  check 'if (y *   -1 =x    ∧    -3   *s<     y  +   3)  then     y:=  v-  3     else 
i:=  3    -   n    ' '⇒ y := (v-3), {}
⇒ skip, {y → -3}'
}
