let abstract_syntax = {|
import {string} from "builtins";

rowType := rowType(list atomicType)

atomicType := string() ???
atomicValue := string(string) ???

row := row(list atomicValue)

table := table(rowType; list row)
|}
