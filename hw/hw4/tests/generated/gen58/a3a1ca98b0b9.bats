load ../../harness

@test "a3a1ca98b0b9" {
  check 'if (¬(1   - z    <   y  +     -3))     then 
x     :=  x    else skip' '⇒ x := x, {}
⇒ skip, {x → 0}'
}
