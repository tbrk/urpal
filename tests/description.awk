# $Id$
#
# 20080722 T. Bourke
#   Original code.
#
# * create eval option for urpal command line:
#	xsltproc --novalid description.xsl testNNN.xml \
#	    | awk -m mode=eval -f description.awk
#
# * create set layout option for urpal command line:
#	xsltproc --novalid description.xsl testNNN.xml \
#	    | awk -m mode=setlayout -f description.awk
#
# * show the test description:
#	xsltproc --novalid description.xsl testNNN.xml \
#	    | awk -m mode=description -f description.awk
#
# * create a diff for stderr:
#	xsltproc --novalid description.xsl testNNN.xml \
#	    | awk -m mode=stderr -f description.awk
#

BEGIN	      { currfield = ""
		field["description"] = ""
		field["error"] = 0
		field["stderr"] = ""
		field["system"] = "system Template, Test;"
		field["layout"] = "fdp"
		field["scale"] = "1.0"
		field["tabulate"] = "no"
		field["author"] = "unknown"
		field["created"] = "00000000"
	      }

/^[ \t]*$/    { next; }
/.*\*\/.*/    { if (mode == "") {
		    for (v in field) printf "%s=%s\n", v, field[v]
		} else if (mode == "eval") {
		    src = "Template"
		    if (field["scale"] != "1.0")
			src = sprintf("scale(Template,%s)", field["scale"])
		    if (field["tabulate"] == "yes")
			src = sprintf("tabulate(%s,{Err})", src)
		    printf("--eval=Test=maketest(%s)\n", src)
		} else if (mode == "setlayout") {
		    printf("--set=graphviz{engine=%s}\n", field["layout"])
		} else {
		    if (field[mode] != "")
			print field[mode]
		}
		exit
	      }

/^[ \t]*(description|error|stderr|layout|scale|tabulate|author|created):/ {
		currfield = $1
		sub(":$", "", currfield)

		value = $0
		sub("^[ \t]*[^:]*:[ \t]*", "", value)
		field[currfield] = value
		next
	      }

(currfield != "") {
		sub("^[ \t]*", "")
		field[currfield] = field[currfield] "\n" $0
	      }
