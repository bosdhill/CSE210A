load ../../harness

@test "f66846a60710" {
  check 'while true     ∧   2    -     -1 <x   +    -3 do   skip   ' '⇒ skip, {}'
}
