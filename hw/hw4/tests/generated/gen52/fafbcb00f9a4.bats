load ../../harness

@test "fafbcb00f9a4" {
  check 'if (¬true)  then  skip    else 
skip' '⇒ skip, {}'
}
