load ../../harness

@test "6d2db5862691" {
  check 'if (¬true) then 

skip   else skip ' '⇒ skip, {}'
}
