load ../../harness

@test "09c75f0d39eb" {
  check 'if (¬(y  -     T   <    a1))  then skip    else    skip     ' '⇒ skip, {}'
}
