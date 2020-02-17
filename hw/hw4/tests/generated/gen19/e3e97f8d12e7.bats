load ../../harness

@test "e3e97f8d12e7" {
  check 'while (¬true)      do     x     :=  0*   -1 ' '⇒ skip, {}'
}
