load ../../harness

@test "59fda8c21efd" {
  check 'skip    ;y :=  -4     -     O' '⇒ y := (-4-O), {}
⇒ skip, {y → -4}'
}
