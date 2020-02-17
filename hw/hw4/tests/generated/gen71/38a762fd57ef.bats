load ../../harness

@test "38a762fd57ef" {
  check 'z   :=   -4    -   z' '⇒ skip, {z → -4}'
}
