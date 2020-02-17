load ../../harness

@test "976cdf5e8c7e" {
  check 'if (true   ∧  -1-   -1<     M  -  y)   then   z :=   3*   k   else skip   ' '⇒ skip, {}'
}
