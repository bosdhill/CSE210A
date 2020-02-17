load ../../harness

@test "4fd5acf341f3" {
  check 'if (false     ∨ y    *   z     =    0 -     z)      then  skip   else skip     ' '⇒ skip, {}'
}
