  V  6   k820309    Ù          2021.9.0    ÙVe                                                                                                          
       E:\workfiles\Githubfiles\dftbplus\build\src\dftbp\geoopt\gdiis.f90 DFTBP_GEOOPT_GDIIS              TDIIS gen@INIT gen@RESET gen@NEXT                                                     
       DP                      @                              
       TDIISMIXER RESET INIT MIX                                                      u #DIISMIXER_INIT    #GDIIS_INIT 
   #         @     @                                               #THIS    #NGENERATION    #INITMIXPARAM    #TFROMSTART    #ALPHA 	                                                   0              #TDIISMIXER              
                                                       
                                      
                
                                                      
                                	     
      #         @     @X                             
                    #THIS    #NELEM    #TOL    #ALPHA    #NGENS              D @                                                  #TDIIS              
                                                       
                                      
                
  @                                   
                
  @                                                                                               u #DIISMIXER_RESET    #GDIIS_RESET    #         @     @                                               #THIS    #NELEM              
                                      0              #TDIISMIXER              
                                             #         @     @X                                                 #THIS    #X              
D @                                                  #TDIIS                                                                 
               &                                                                                                  u #GDIIS_NEXT    #         @     @X                                                 #THIS    #DX    #XNEW    #TCONVERGED              
D @                                                  #TDIIS              
  @                                                 
              &                                                     D                                                   
               &                                                     D                                                           @                                      u #DIISMIXER_MIX    #         @     @                                               #THIS    #QINPRESULT    #QDIFF              
                                      0              #TDIISMIXER              
                                                   
               &                                                     
                                                    
              &                                                             @              @                '0                   #INITMIXPARAM     #MPREVVECTOR !   #IPREVVECTOR "   #NELEM #   #INDX $   #PREVQINPUT %   #PREVQDIFF &   #TFROMSTART '   #TADDINTRPGRADIENT (   #ALPHA )   #DELTAR *                 D                                              
                 D                              !                                D                              "                                D                              #                                D                              $                              D                             %                             
            &                   &                                                       D                             &            x                 
            &                   &                                                         D                              '     Ø                           D                              (     Ü       	                    D                             )     à       
   
               D                             *            è                 
            &                                                             @               À                '                   #PDIIS +   #X ,   #NELEM -   #TOLERANCE .   #TINITIALIZED /                 D                              +     0                     #TDIISMIXER                D                             ,            0                
            &                                                         D                              -     x                          D                             .              
                 D                              /                 *          n                                       Cifmodintr.lib   n                                         Cifmodintr.lib                                               ^      fn#fn (   þ   2   b   uapp(DFTBP_GEOOPT_GDIIS &   0  C   J  DFTBP_COMMON_ACCURACY &   s  Z   J  DFTBP_MIXER_DIISMIXER    Í  d       gen@INIT 5   1        DIISMIXER_INIT+DFTBP_MIXER_DIISMIXER :   Á  X   a   DIISMIXER_INIT%THIS+DFTBP_MIXER_DIISMIXER A     @   a   DIISMIXER_INIT%NGENERATION+DFTBP_MIXER_DIISMIXER B   Y  @   a   DIISMIXER_INIT%INITMIXPARAM+DFTBP_MIXER_DIISMIXER @     @   a   DIISMIXER_INIT%TFROMSTART+DFTBP_MIXER_DIISMIXER ;   Ù  @   a   DIISMIXER_INIT%ALPHA+DFTBP_MIXER_DIISMIXER      |      GDIIS_INIT       S   a   GDIIS_INIT%THIS !   è  @   a   GDIIS_INIT%NELEM    (  @   a   GDIIS_INIT%TOL !   h  @   a   GDIIS_INIT%ALPHA !   ¨  @   a   GDIIS_INIT%NGENS    è  f       gen@RESET 6   N  ]      DIISMIXER_RESET+DFTBP_MIXER_DIISMIXER ;   «  X   a   DIISMIXER_RESET%THIS+DFTBP_MIXER_DIISMIXER <     @   a   DIISMIXER_RESET%NELEM+DFTBP_MIXER_DIISMIXER    C  Y      GDIIS_RESET !     S   a   GDIIS_RESET%THIS    ï     a   GDIIS_RESET%X    {  P       gen@NEXT    Ë  t      GDIIS_NEXT     ?	  S   a   GDIIS_NEXT%THIS    	     a   GDIIS_NEXT%DX     
     a   GDIIS_NEXT%XNEW &   ª
  @   a   GDIIS_NEXT%TCONVERGED .   ê
  S       gen@MIX+DFTBP_MIXER_DIISMIXER 4   =  m      DIISMIXER_MIX+DFTBP_MIXER_DIISMIXER 9   ª  X   a   DIISMIXER_MIX%THIS+DFTBP_MIXER_DIISMIXER ?        a   DIISMIXER_MIX%QINPRESULT+DFTBP_MIXER_DIISMIXER :        a   DIISMIXER_MIX%QDIFF+DFTBP_MIXER_DIISMIXER 1     ö       TDIISMIXER+DFTBP_MIXER_DIISMIXER K     H   %   TDIISMIXER%INITMIXPARAM+DFTBP_MIXER_DIISMIXER=INITMIXPARAM I   X  H   %   TDIISMIXER%MPREVVECTOR+DFTBP_MIXER_DIISMIXER=MPREVVECTOR I      H   %   TDIISMIXER%IPREVVECTOR+DFTBP_MIXER_DIISMIXER=IPREVVECTOR =   è  H   %   TDIISMIXER%NELEM+DFTBP_MIXER_DIISMIXER=NELEM ;   0  H   %   TDIISMIXER%INDX+DFTBP_MIXER_DIISMIXER=INDX G   x  ¬   %   TDIISMIXER%PREVQINPUT+DFTBP_MIXER_DIISMIXER=PREVQINPUT E   $  ¬   %   TDIISMIXER%PREVQDIFF+DFTBP_MIXER_DIISMIXER=PREVQDIFF G   Ð  H   %   TDIISMIXER%TFROMSTART+DFTBP_MIXER_DIISMIXER=TFROMSTART U     H   %   TDIISMIXER%TADDINTRPGRADIENT+DFTBP_MIXER_DIISMIXER=TADDINTRPGRADIENT =   `  H   %   TDIISMIXER%ALPHA+DFTBP_MIXER_DIISMIXER=ALPHA ?   ¨     %   TDIISMIXER%DELTAR+DFTBP_MIXER_DIISMIXER=DELTAR    <         TDIIS    Ê  `   !   TDIIS%PDIIS    *     !   TDIIS%X    ¾  H   !   TDIIS%NELEM       H   !   TDIIS%TOLERANCE #   N  H   !   TDIIS%TINITIALIZED      À      MsObjComment 