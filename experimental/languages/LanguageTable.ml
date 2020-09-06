let abstract_syntax =
  {|
{ list, string }

rowType := rowType(list(atomicType()))

atomicType := string() ???
atomicValue := string(string) ???

row := row(list(atomicValue))

table := table(rowType; list(row))
|}
;;
