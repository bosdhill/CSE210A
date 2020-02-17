load ../../harness

@test "6143bcea9ef7" {
  check 'while (¬true)    do   s    := z*z ' '⇒ skip, {}'
}
