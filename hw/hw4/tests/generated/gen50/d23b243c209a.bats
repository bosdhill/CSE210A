load ../../harness

@test "d23b243c209a" {
  check 'if (true   ∨     true)   then skip   else skip     ' '⇒ skip, {}'
}
