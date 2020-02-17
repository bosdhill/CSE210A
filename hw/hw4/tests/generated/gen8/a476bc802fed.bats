load ../../harness

@test "a476bc802fed" {
  check 'if (true ∧    0-   4    <     -2    +   -1)     then  

skip    else skip  ' '⇒ skip, {}'
}
