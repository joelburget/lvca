let abstract_syntax =
  {|
import {sequence, string} from "builtins";

rowType := rowType(sequence(atomicType()))

atomicType := string() ???
atomicValue := string(string) ???

row := row(sequence(atomicValue))

table := table(rowType; sequence(row))
|}
;;
