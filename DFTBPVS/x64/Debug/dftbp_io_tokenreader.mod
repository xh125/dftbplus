  '"  W   k820309    �          2021.9.0    �Ve                                                                                                          
       E:\workfiles\Githubfiles\dftbplus\build\src\dftbp\io\tokenreader.f90 DFTBP_IO_TOKENREADER              TOKEN_OK TOKEN_EOS TOKEN_ERROR LOGICAL_TRUE LOGICAL_FALSE LOGICAL_TRUE_LO LOGICAL_FALSE_LO gen@GETNEXTTOKEN                                                     
       DP                                                     
      STRING i@|                      @                              
       WHITESPACES TOLOWER COMPLEMENTARYSCAN UNQUOTEDSCAN                      @                              
       ERROR                                                    	   u #GETNEXTTOKEN_STRING    #GETNEXTTOKEN_INTEGER    #GETNEXTTOKEN_INTEGERR1    #GETNEXTTOKEN_REAL    #GETNEXTTOKEN_REALR1    #GETNEXTTOKEN_COMPLEX !   #GETNEXTTOKEN_COMPLEXR1 &   #GETNEXTTOKEN_LOGICAL ,   #GETNEXTTOKEN_LOGICALR1 1   #         @     @X                                                 #STR    #TOKENVALUE    #START 	   #IOSTAT 
             
  @                                                 1           
D @                                    P               #STRING              
D @                               	                      F @                               
            #         @     @X                                                #STR    #TOKENVALUE    #START    #IOSTAT              
  @                                                 1           D                                                       
D                                                       F @                                           #         @     @X                                                 #STR    #TOKENVALUE    #START    #IOSTAT    #NITEM              
  @                                                 1           D@                                                                 &                                                     
D                                                       F @                                                     F @                                           #         @     @X                                                #STR    #TOKENVALUE    #START    #IOSTAT              
  @                                                 1           D                                     
                 
D                                                       F @                                           #         @     @X                                                 #STR    #TOKENVALUE    #START    #IOSTAT    #NITEM               
  @                                                 1           D@                                                 
               &                                                     
D                                                       F @                                                     F @                                            #         @     @X                            !                    #STR "   #TOKENVALUE #   #START $   #IOSTAT %             
  @                             "                    1           D                                #                      
D                                 $                      F @                               %            #         @     @X                             &                    #STR '   #TOKENVALUE (   #START )   #IOSTAT *   #NITEM +             
  @                             '                    1           D@                              (                                  &                                                     
D                                 )                      F @                               *                      F @                               +            #         @     @X                            ,                    #STR -   #TOKENVALUE .   #START /   #IOSTAT 0             
  @                             -                    1           D                                 .                      
D                                 /                      F @                               0            #         @     @X                             1                    #STR 2   #TOKENVALUE 3   #START 4   #IOSTAT 5   #NITEM 6             
  @                             2                    1           D@                               3                                  &                                                     
D                                 4                      F @                               5                      F @                               6                                                                      |  #ASSIGN_S_TO_S 7   #ASSIGN_S_TO_C :   #ASSIGN_C_TO_S =   #         H     @                           7                    #VAR 8   #EXPR 9             
                                 8     P               #STRING              
                                  9     P              #STRING    #         H     @                           :                    #VAR ;   #EXPR <                                            ;                     1           
                                  <     P              #STRING    #         H     @                          =                    #VAR >   #EXPR ?             
                                 >     P               #STRING              
                                ?                    1               @�                                      u #ERROR_SINGLE @   #ERROR_ARRAY B   #         @     @                           @                    #MESSAGE A             
                                A                    1 #         @     @                            B                    #MESSAGES C   ,          
                               C                                  &                                           1                �  @              A                'P                    #LEN D   #SIZE E   #CHARS F               � D                              D                                                                                                 0                � D                              E                                                                                                0    .           � D                              F                                         &                                                                  �  @             E           G     'P                    #LEN H   #SIZE I   #CHARS J               � D                             H                                                                                                 0                � D                             I                                                                                                0    .           � D                             J                                         &                                                                                                K                                                       0                                             L                                          ��������                                                     M                                          ��������                                                   N                                                        CYes                                                           O                                                        CNo                                                           P                                                        Cyes                                                           Q                                                        Cno                *         � n                         �              Cifmodintr.lib  � n                           �              Cifmodintr.lib                                           �   b      fn#fn *     |   b   uapp(DFTBP_IO_TOKENREADER &   ~  C   J  DFTBP_COMMON_ACCURACY %   �  K   J  DFTBP_EXTLIBS_XMLF90 #     s   J  DFTBP_IO_CHARMANIP !     F   J  DFTBP_IO_MESSAGE !   �  +      gen@GETNEXTTOKEN $   �  x      GETNEXTTOKEN_STRING (   h  L   a   GETNEXTTOKEN_STRING%STR /   �  T   a   GETNEXTTOKEN_STRING%TOKENVALUE *     @   a   GETNEXTTOKEN_STRING%START +   H  @   a   GETNEXTTOKEN_STRING%IOSTAT %   �  x      GETNEXTTOKEN_INTEGER )      L   a   GETNEXTTOKEN_INTEGER%STR 0   L  @   a   GETNEXTTOKEN_INTEGER%TOKENVALUE +   �  @   a   GETNEXTTOKEN_INTEGER%START ,   �  @   a   GETNEXTTOKEN_INTEGER%IOSTAT '     �      GETNEXTTOKEN_INTEGERR1 +   �  L   a   GETNEXTTOKEN_INTEGERR1%STR 2   �  �   a   GETNEXTTOKEN_INTEGERR1%TOKENVALUE -   g  @   a   GETNEXTTOKEN_INTEGERR1%START .   �  @   a   GETNEXTTOKEN_INTEGERR1%IOSTAT -   �  @   a   GETNEXTTOKEN_INTEGERR1%NITEM "   '	  x      GETNEXTTOKEN_REAL &   �	  L   a   GETNEXTTOKEN_REAL%STR -   �	  @   a   GETNEXTTOKEN_REAL%TOKENVALUE (   +
  @   a   GETNEXTTOKEN_REAL%START )   k
  @   a   GETNEXTTOKEN_REAL%IOSTAT $   �
  �      GETNEXTTOKEN_REALR1 (   .  L   a   GETNEXTTOKEN_REALR1%STR /   z  �   a   GETNEXTTOKEN_REALR1%TOKENVALUE *     @   a   GETNEXTTOKEN_REALR1%START +   F  @   a   GETNEXTTOKEN_REALR1%IOSTAT *   �  @   a   GETNEXTTOKEN_REALR1%NITEM %   �  x      GETNEXTTOKEN_COMPLEX )   >  L   a   GETNEXTTOKEN_COMPLEX%STR 0   �  @   a   GETNEXTTOKEN_COMPLEX%TOKENVALUE +   �  @   a   GETNEXTTOKEN_COMPLEX%START ,   
  @   a   GETNEXTTOKEN_COMPLEX%IOSTAT '   J  �      GETNEXTTOKEN_COMPLEXR1 +   �  L   a   GETNEXTTOKEN_COMPLEXR1%STR 2     �   a   GETNEXTTOKEN_COMPLEXR1%TOKENVALUE -   �  @   a   GETNEXTTOKEN_COMPLEXR1%START .   �  @   a   GETNEXTTOKEN_COMPLEXR1%IOSTAT -   %  @   a   GETNEXTTOKEN_COMPLEXR1%NITEM %   e  x      GETNEXTTOKEN_LOGICAL )   �  L   a   GETNEXTTOKEN_LOGICAL%STR 0   )  @   a   GETNEXTTOKEN_LOGICAL%TOKENVALUE +   i  @   a   GETNEXTTOKEN_LOGICAL%START ,   �  @   a   GETNEXTTOKEN_LOGICAL%IOSTAT '   �  �      GETNEXTTOKEN_LOGICALR1 +   l  L   a   GETNEXTTOKEN_LOGICALR1%STR 2   �  �   a   GETNEXTTOKEN_LOGICALR1%TOKENVALUE -   D  @   a   GETNEXTTOKEN_LOGICALR1%START .   �  @   a   GETNEXTTOKEN_LOGICALR1%IOSTAT -   �  @   a   GETNEXTTOKEN_LOGICALR1%NITEM      y      i@| -   }  [      ASSIGN_S_TO_S+XMLF90_STRINGS 1   �  T   a   ASSIGN_S_TO_S%VAR+XMLF90_STRINGS 2   ,  T   a   ASSIGN_S_TO_S%EXPR+XMLF90_STRINGS -   �  [      ASSIGN_S_TO_C+XMLF90_STRINGS 1   �  L   a   ASSIGN_S_TO_C%VAR+XMLF90_STRINGS 2   '  T   a   ASSIGN_S_TO_C%EXPR+XMLF90_STRINGS -   {  [      ASSIGN_C_TO_S+XMLF90_STRINGS 1   �  T   a   ASSIGN_C_TO_S%VAR+XMLF90_STRINGS 2   *  L   a   ASSIGN_C_TO_S%EXPR+XMLF90_STRINGS +   v  c       gen@ERROR+DFTBP_IO_MESSAGE .   �  U      ERROR_SINGLE+DFTBP_IO_MESSAGE 6   .  L   a   ERROR_SINGLE%MESSAGE+DFTBP_IO_MESSAGE -   z  V      ERROR_ARRAY+DFTBP_IO_MESSAGE 6   �  �   a   ERROR_ARRAY%MESSAGES+DFTBP_IO_MESSAGE &   `  n       STRING+XMLF90_STRINGS .   �  �   %   STRING%LEN+XMLF90_STRINGS=LEN 0   s  �   %   STRING%SIZE+XMLF90_STRINGS=SIZE 2     �   %   STRING%CHARS+XMLF90_STRINGS=CHARS &   �  n      STRING+XMLF90_STRINGS .   "  �   %   STRING%LEN+XMLF90_STRINGS=LEN 0   �  �   %   STRING%SIZE+XMLF90_STRINGS=SIZE 2   l  �   %   STRING%CHARS+XMLF90_STRINGS=CHARS      q       TOKEN_OK    y  p       TOKEN_EOS    �  p       TOKEN_ERROR    Y  �       LOGICAL_TRUE    �  �       LOGICAL_FALSE     `   �       LOGICAL_TRUE_LO !   �   �       LOGICAL_FALSE_LO    g!  �      MsObjComment 