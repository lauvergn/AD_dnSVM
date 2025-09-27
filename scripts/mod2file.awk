{
   n=split(FILENAME,tab,"/")
   if (n > 0) {
      l=length(tab[n])
      objf = "$(OBJ_DIR)/" substr(tab[n],1,l-4) ".o"
   }
   if (tolower($1) == "module" && tolower($2) != "procedure") {
            print tolower($2) " := " objf
   }
}