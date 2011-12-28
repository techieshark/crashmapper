IN:=report

report: 
	cat $@.lhs | pandoc --no-wrap -sS -c pandoc.css -f markdown+lhs > $@.html
