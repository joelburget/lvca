#! /bin/sh

for i in $(rg -l "$1"); do
	sed -i '' -e "s/$1/$2/g" $i
done
