load ../../harness

@test "5e5bfec60189" {
  check 'while 0     + 2    =     z  -  1 do skip     ' '⇒ skip, {}'
}
