load ../../harness

@test "5827e5383b0a" {
  check 'if (x     -    Sd=-40534570491048060125007000879794943779    *     -31126750291691130286966099392336131830    ∧ false)  then 
 skip   else skip' '⇒ skip, {}'
}
