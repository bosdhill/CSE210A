load ../../harness

@test "b93a1ffa2e47" {
  check 'while (¬true)     do  z :=     z   *  y  ' '⇒ skip, {}'
}
