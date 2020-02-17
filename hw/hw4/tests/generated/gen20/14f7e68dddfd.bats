load ../../harness

@test "14f7e68dddfd" {
  check 'if (¬(0  *     y < z   +    3))     then x  :=   4   --4   else skip    ' '⇒ skip, {}'
}
