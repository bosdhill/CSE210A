load ../../harness

@test "4587716d49d4" {
  check 'if false    then skip    else skip   ' '⇒ skip, {}'
}
