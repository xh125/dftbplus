  !  1   k820309    Ù          2021.9.0    ÙVe                                                                                                          
       E:\workfiles\Githubfiles\dftbplus\build\src\dftbp\dftb\slakoeqgrid.f90 DFTBP_DFTB_SLAKOEQGRID              TSLAKOEQGRID SKEQGRIDOLD SKEQGRIDNEW gen@INIT gen@GETSKINTEGRALS gen@GETNINTEGRALS gen@GETCUTOFF                                                     
       DP DISTFUDGE DISTFUDGEOLD                      @                              
       ERROR                      @                              
       POLYINTERUNIFORM POLY5TOZERO FREECUBICSPLINE                                                        u #SLAKOEQGRID_INIT    #         @     @X                                                 #THIS    #DIST    #TABLE    #SKINTMETHOD 	             D                                      x               #TSLAKOEQGRID              
                                      
                
 @                                                 
              &                   &                                                     
                                  	                                                                  u #SLAKOEQGRID_GETSKINTEGRALS 
   #         @     @X                             
                    #THIS    #SK    #DIST              
  @                                    x              #TSLAKOEQGRID              D @                                                 
               &                                                     
  @                                   
                                                             u #SLAKOEQGRID_GETNINTEGRALS    %         @    @X                                                       #THIS              
                                       x              #TSLAKOEQGRID                                                           u #SLAKOEQGRID_GETCUTOFF    %         @    @X                                                
       #THIS              
                                       x              #TSLAKOEQGRID                  @                                      u #ERROR_SINGLE    #ERROR_ARRAY    #         @     @                                               #MESSAGE              
                                                    1 #         @     @                                                #MESSAGES    ,          
                                                                 &                                           1               @                                       u #POLYINTERU1    #POLYINTERU2    %         @    @                                               
       #XP    #YP    #XX    #DY              
                                                   
              &                                                     
                                                    
              &                                                     
                                      
                                                     
       (        `   @                                                              
    #XP    #YP    #XX    #DY    p          H r       7
S
O
 p        j                      j                        n                                           1                H r       7
S
O
 p        j                      j                        n                                          1                                         
                                                   
              &                                                     
                                                   
              &                   &                                                     
                                      
                                                                   
               &                                                             @               A                'x                    #NGRID !   #NINTEG "   #DIST #   #SKTAB $   #SKINTMETHOD %   #TINIT &                 D                              !                                D                              "                                D                             #               
               D                             $                             
            &                   &                                                         D                              %     p                          D                              &     t                                                                                                                                               '                                                      1                                             (                                                      2              @                                 SIZE *          n                         ³              Cifmodintr.lib   n                           ³              Cifmodintr.lib                                               f      fn#fn ,     q   b   uapp(DFTBP_DFTB_SLAKOEQGRID &   w  Z   J  DFTBP_COMMON_ACCURACY !   Ñ  F   J  DFTBP_IO_MESSAGE )     m   J  DFTBP_MATH_INTERPOLATION      V       gen@INIT !   Ú  x      SLAKOEQGRID_INIT &   R  Z   a   SLAKOEQGRID_INIT%THIS &   ¬  @   a   SLAKOEQGRID_INIT%DIST '   ì  ¤   a   SLAKOEQGRID_INIT%TABLE -     @   a   SLAKOEQGRID_INIT%SKINTMETHOD #   Ð  `       gen@GETSKINTEGRALS +   0  d      SLAKOEQGRID_GETSKINTEGRALS 0     Z   a   SLAKOEQGRID_GETSKINTEGRALS%THIS .   î     a   SLAKOEQGRID_GETSKINTEGRALS%SK 0   z  @   a   SLAKOEQGRID_GETSKINTEGRALS%DIST "   º  _       gen@GETNINTEGRALS *     Z      SLAKOEQGRID_GETNINTEGRALS /   s  Z   a   SLAKOEQGRID_GETNINTEGRALS%THIS    Í  [       gen@GETCUTOFF &   (  Z      SLAKOEQGRID_GETCUTOFF +     Z   a   SLAKOEQGRID_GETCUTOFF%THIS +   Ü  c       gen@ERROR+DFTBP_IO_MESSAGE .   ?	  U      ERROR_SINGLE+DFTBP_IO_MESSAGE 6   	  L   a   ERROR_SINGLE%MESSAGE+DFTBP_IO_MESSAGE -   à	  V      ERROR_ARRAY+DFTBP_IO_MESSAGE 6   6
     a   ERROR_ARRAY%MESSAGES+DFTBP_IO_MESSAGE >   Æ
  b       gen@POLYINTERUNIFORM+DFTBP_MATH_INTERPOLATION 5   (  p      POLYINTERU1+DFTBP_MATH_INTERPOLATION 8        a   POLYINTERU1%XP+DFTBP_MATH_INTERPOLATION 8   $     a   POLYINTERU1%YP+DFTBP_MATH_INTERPOLATION 8   °  @   a   POLYINTERU1%XX+DFTBP_MATH_INTERPOLATION 8   ð  @   a   POLYINTERU1%DY+DFTBP_MATH_INTERPOLATION 5   0       POLYINTERU2+DFTBP_MATH_INTERPOLATION 8   >     a   POLYINTERU2%XP+DFTBP_MATH_INTERPOLATION 8   Ê  ¤   a   POLYINTERU2%YP+DFTBP_MATH_INTERPOLATION 8   n  @   a   POLYINTERU2%XX+DFTBP_MATH_INTERPOLATION 8   ®     a   POLYINTERU2%DY+DFTBP_MATH_INTERPOLATION    :         TSLAKOEQGRID #   Ò  H   !   TSLAKOEQGRID%NGRID $     H   !   TSLAKOEQGRID%NINTEG "   b  H   !   TSLAKOEQGRID%DIST #   ª  ¬   !   TSLAKOEQGRID%SKTAB )   V  H   !   TSLAKOEQGRID%SKINTMETHOD #     ¤   !   TSLAKOEQGRID%TINIT    B  q       SKEQGRIDOLD    ³  q       SKEQGRIDNEW ?   $  =      POLYINTERU2%SIZE+DFTBP_MATH_INTERPOLATION=SIZE    a  À      MsObjComment 