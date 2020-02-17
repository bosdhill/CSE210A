load ../../harness

@test "f625ee4fa1b3" {
  check 'while (¬true)   do   z  :=iK    *  z ' '⇒ skip, {}'
}
