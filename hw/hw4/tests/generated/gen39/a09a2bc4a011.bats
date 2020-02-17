load ../../harness

@test "a09a2bc4a011" {
  check 'while x -    -3    <3   - 1   ∧    y   -3  =    z   *     2    do  z:= y   -    x    ' '⇒ skip, {}'
}
