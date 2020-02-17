load ../../harness

@test "376cf31a7264" {
  check 'if (¬true)    then 
skip else skip' '⇒ skip, {}'
}
