load ../../harness

@test "841be83ea386" {
  check 'if (-1   -4    <3   *     2    ∨  true)  then 
skip else skip  ' '⇒ skip, {}'
}
