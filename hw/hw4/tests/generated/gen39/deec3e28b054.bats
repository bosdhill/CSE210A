load ../../harness

@test "deec3e28b054" {
  check 'if (¬(-4  +     -2  <q   - 1))  then z   :=x- x else skip     ' '⇒ skip, {}'
}
