BEGIN {imod=0} 
{
  if (tolower($1) == "use") add_name(tolower($2))
}
END {
  #print "file: " FILENAME
  #print "imod: " imod

  n=split(FILENAME,tab,"/")
  if (n > 0) {
    #print n " " tab[n]
    l=length(tab[n])
    objf = "$(OBJ_DIR)/" substr(tab[n],1,l-4) ".o"
  }
  #for (i=1;i<imod+1;i=i+1) {print i " " tab_mod[i]}
  if (imod > 0)  {
    print objf " : \\"
    for (i=1;i<imod;i=i+1) print "          $(" tab_mod[i] ") \\"
    print "          $(" tab_mod[imod] ")"
  }
}
function add_name(name) {
  # keep the string part before "," (if any)
  if (index(name,",") > 0) {
    n=split(name,tab,",")
    name=tab[1]
  }
  #
  find=0
  #print "imod:" imod " name: " name
  if (imod > 0) {
      for (j=1;j<=imod;j=j+1) {
        #print "j:" j " tab_mod: " tab_mod[j]
        if (tab_mod[j] == name) {find=1;break}
      }
  }
  #print " imod:" imod " find: " find
  if (find == 0) {
    imod=imod+1
    tab_mod[imod]=name
    #print "add name: " tab_mod[imod] " imod: " imod
    }
  #for (j=1;j<=i;j=j+1) print "end j:" j " tab_mod: " tab_mod[j]
  #print "-------------------------------------"
}