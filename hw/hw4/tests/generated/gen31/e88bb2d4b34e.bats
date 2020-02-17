load ../../harness

@test "e88bb2d4b34e" {
  check 'if (¬(x   + -2  <     x))  then skip else B7   :=   C  *   -4  ' '⇒ B7 := (C*-4), {}
⇒ skip, {B7 → 0}'
}
