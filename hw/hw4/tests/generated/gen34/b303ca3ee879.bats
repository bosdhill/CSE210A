load ../../harness

@test "b303ca3ee879" {
  check 'if (2   -  x    <    hn  ∧   4     -    4<   1)    then skip     else  skip    ' '⇒ skip, {}'
}
