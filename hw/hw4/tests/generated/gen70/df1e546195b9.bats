load ../../harness

@test "df1e546195b9" {
  check 'if (true    ∧   false)  then 
  skip    else skip     ' '⇒ skip, {}'
}
