load ../../harness

@test "7c50ee821bd5" {
  check 'if (y     * 1<     3+  y∨     1     +-4    <  x*2)   then 
  y := y    else x:=  -2     *x' '⇒ y := y, {}
⇒ skip, {y → 0}'
}
