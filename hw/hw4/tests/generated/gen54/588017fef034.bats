load ../../harness

@test "588017fef034" {
  check 'if (4 *  1=     -1  + 2 ∧    true) then skip   else  skip  ' '⇒ skip, {}'
}
