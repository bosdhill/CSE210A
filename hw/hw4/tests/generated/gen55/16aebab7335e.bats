load ../../harness

@test "16aebab7335e" {
  check 'while -1 *  -4= 3    +3 do skip   ' '⇒ skip, {}'
}
