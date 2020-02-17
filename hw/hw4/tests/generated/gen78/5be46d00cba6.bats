load ../../harness

@test "5be46d00cba6" {
  check 'while (¬(KI     =  y  * -1))   do   y    :=  -4   *   z     ' '⇒ skip, {}'
}
