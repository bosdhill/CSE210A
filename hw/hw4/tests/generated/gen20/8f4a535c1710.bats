load ../../harness

@test "8f4a535c1710" {
  check 'while (¬(z     *0    = z))   do skip     ' '⇒ skip, {}'
}
