load ../../harness

@test "a991be9329d0" {
  check 'if (true  ∧ 2    +2    =    ZG     *     4)     then skip    else    skip' '⇒ skip, {}'
}
