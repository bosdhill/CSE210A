load ../../harness

@test "10cae06f6787" {
  check 'if (y     *-1= 0*-2)   then skip     else   skip' '⇒ skip, {}'
}
