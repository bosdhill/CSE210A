load ../../harness

@test "9941dd688741" {
  check 'while false do skip   ' '⇒ skip, {}'
}
