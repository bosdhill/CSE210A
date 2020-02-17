load ../../harness

@test "95c90df26bff" {
  check 'y   :=  4     -    -3' '⇒ skip, {y → 7}'
}
