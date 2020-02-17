load ../../harness

@test "206598a57d1b" {
  check 'x:= x *    2-     y   ' '⇒ skip, {x → 0}'
}
