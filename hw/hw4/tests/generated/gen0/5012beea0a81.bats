load ../../harness

@test "5012beea0a81" {
  check 'y   :=   z     - 4 ' '⇒ skip, {y → -4}'
}
