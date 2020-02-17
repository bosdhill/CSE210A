load ../../harness

@test "8148936c531a" {
  check 'if (i     * 4   <   z     -   z ∧    f0 +     -3=y  -   4)    then e := 4    else  skip' '⇒ skip, {}'
}
