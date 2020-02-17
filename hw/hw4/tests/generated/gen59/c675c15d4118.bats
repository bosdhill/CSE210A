load ../../harness

@test "c675c15d4118" {
  check 'while (¬true)     do   y     :=   x +   4    ' '⇒ skip, {}'
}
