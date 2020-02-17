load ../../harness

@test "033aae6be920" {
  check 'while (¬(2  *z  =     z*     1))     do y := -2' '⇒ skip, {}'
}
