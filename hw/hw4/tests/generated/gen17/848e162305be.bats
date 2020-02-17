load ../../harness

@test "848e162305be" {
  check 'if (¬true)    then skip   else wC    :=    4   + 0     ' '⇒ wC := (4+0), {}
⇒ skip, {wC → 4}'
}
