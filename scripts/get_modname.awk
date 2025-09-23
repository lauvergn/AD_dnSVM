{
   if (tolower($1) == "module" && tolower($2) != "procedure") {
            print tolower($2)
   }
}