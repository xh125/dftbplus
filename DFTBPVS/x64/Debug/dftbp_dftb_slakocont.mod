  Ú  6   k820309    Ù          2021.9.0    ÙVe                                                                                                          
       E:\workfiles\Githubfiles\dftbplus\build\src\dftbp\dftb\slakocont.f90 DFTBP_DFTB_SLAKOCONT              TSLAKOCONT gen@GETCUTOFF gen@GETSKINTEGRALS gen@INIT gen@ADDTABLE gen@GETMINTEGRALS                                                     
       DP                      @                              
       TSLAKOEQGRID GETSKINTEGRALS GETNINTEGRALS GETCUTOFF                                                       u #SLAKOEQGRID_GETCUTOFF    #SLAKOCONT_GETCUTOFF    %         @    @                                              
       #THIS              
                                       x              #TSLAKOEQGRID    %         @    @X                                                
       #THIS              
                                       x              #TSLAKOCONT                                                         u #SLAKOEQGRID_GETSKINTEGRALS 	   #SLAKOCONT_GETSKINTEGRALS    #         @     @                           	                    #THIS 
   #SK    #DIST              
                                  
     x              #TSLAKOEQGRID                                                                 
               &                                                     
                                      
      #         @     @X                                                 #THIS    #SK    #DIST    #SP1    #SP2              
  @                                    x              #TSLAKOCONT              D @                                                 
               &                                                     
  @                                   
                
                                                       
                                                                                                    u #SLAKOCONT_INIT    #         @     @X                                                 #THIS    #NSPECIES              D                                      x               #TSLAKOCONT              
                                                                                                    u #SLAKOCONT_ADDTABLEEQGRID    #         @     @X                                                 #THIS    #PTABLE    #ISP1    #ISP2              
D                                      x               #TSLAKOCONT             
 @                                    x               #TSLAKOEQGRID              
                                                       
                                                                                                    u #SLAKOCONT_GETMINTEGRALS    %         @    @X                                                      #THIS              
                                       x              #TSLAKOCONT                  @                                       u #SLAKOEQGRID_GETNINTEGRALS    %         @    @                                                     #THIS              
                                       x              #TSLAKOEQGRID                      @              A                'x                    #NGRID    #NINTEG     #DIST !   #SKTAB "   #SKINTMETHOD #   #TINIT $                 D                                                               D                                                               D                             !               
               D                             "                             
            &                   &                                                         D                              #     p                          D                              $     t                                                                                                                    @               Á                'x                    #SLAKOS %   #NSPECIES )   #MINT *   #CUTOFF +   #TDATAOK ,   #TINIT -               D                              %                                 #TSLAKO_ &             &                   &                                                          @  @             Á           &     '                    #ITYPE '   #PSLAKOEQGRID (                                              '                                                                                                 0                                               (     x                     #TSLAKOEQGRID                  D                              )     `                           D                              *     d                           D                             +     h          
                 D                              ,     p                          D                              -     t                                                                                                  *          n                                       Cifmodintr.lib   n                                         Cifmodintr.lib                                               b      fn#fn *     d   b   uapp(DFTBP_DFTB_SLAKOCONT &   f  C   J  DFTBP_COMMON_ACCURACY '   ©  t   J  DFTBP_DFTB_SLAKOEQGRID      t       gen@GETCUTOFF =     Z      SLAKOEQGRID_GETCUTOFF+DFTBP_DFTB_SLAKOEQGRID B   ë  Z   a   SLAKOEQGRID_GETCUTOFF%THIS+DFTBP_DFTB_SLAKOEQGRID $   E  Z      SLAKOCONT_GETCUTOFF )     X   a   SLAKOCONT_GETCUTOFF%THIS #   ÷  ~       gen@GETSKINTEGRALS B   u  d      SLAKOEQGRID_GETSKINTEGRALS+DFTBP_DFTB_SLAKOEQGRID G   Ù  Z   a   SLAKOEQGRID_GETSKINTEGRALS%THIS+DFTBP_DFTB_SLAKOEQGRID E   3     a   SLAKOEQGRID_GETSKINTEGRALS%SK+DFTBP_DFTB_SLAKOEQGRID G   ¿  @   a   SLAKOEQGRID_GETSKINTEGRALS%DIST+DFTBP_DFTB_SLAKOEQGRID )   ÿ  v      SLAKOCONT_GETSKINTEGRALS .   u  X   a   SLAKOCONT_GETSKINTEGRALS%THIS ,   Í     a   SLAKOCONT_GETSKINTEGRALS%SK .   Y  @   a   SLAKOCONT_GETSKINTEGRALS%DIST -     @   a   SLAKOCONT_GETSKINTEGRALS%SP1 -   Ù  @   a   SLAKOCONT_GETSKINTEGRALS%SP2      T       gen@INIT    m  `      SLAKOCONT_INIT $   Í  X   a   SLAKOCONT_INIT%THIS (   %	  @   a   SLAKOCONT_INIT%NSPECIES    e	  ^       gen@ADDTABLE )   Ã	  r      SLAKOCONT_ADDTABLEEQGRID .   5
  X   a   SLAKOCONT_ADDTABLEEQGRID%THIS 0   
  Z   a   SLAKOCONT_ADDTABLEEQGRID%PTABLE .   ç
  @   a   SLAKOCONT_ADDTABLEEQGRID%ISP1 .   '  @   a   SLAKOCONT_ADDTABLEEQGRID%ISP2 "   g  ]       gen@GETMINTEGRALS (   Ä  Z      SLAKOCONT_GETMINTEGRALS -     X   a   SLAKOCONT_GETMINTEGRALS%THIS 9   v  _       gen@GETNINTEGRALS+DFTBP_DFTB_SLAKOEQGRID A   Õ  Z      SLAKOEQGRID_GETNINTEGRALS+DFTBP_DFTB_SLAKOEQGRID F   /  Z   a   SLAKOEQGRID_GETNINTEGRALS%THIS+DFTBP_DFTB_SLAKOEQGRID 4            TSLAKOEQGRID+DFTBP_DFTB_SLAKOEQGRID @   !  H   %   TSLAKOEQGRID%NGRID+DFTBP_DFTB_SLAKOEQGRID=NGRID B   i  H   %   TSLAKOEQGRID%NINTEG+DFTBP_DFTB_SLAKOEQGRID=NINTEG >   ±  H   %   TSLAKOEQGRID%DIST+DFTBP_DFTB_SLAKOEQGRID=DIST @   ù  ¬   %   TSLAKOEQGRID%SKTAB+DFTBP_DFTB_SLAKOEQGRID=SKTAB L   ¥  H   %   TSLAKOEQGRID%SKINTMETHOD+DFTBP_DFTB_SLAKOEQGRID=SKINTMETHOD @   í  ¤   %   TSLAKOEQGRID%TINIT+DFTBP_DFTB_SLAKOEQGRID=TINIT             TSLAKOCONT "   )  ¹   !   TSLAKOCONT%SLAKOS    â  m      TSLAKO_    O  ¥   a   TSLAKO_%ITYPE %   ô  b   a   TSLAKO_%PSLAKOEQGRID $   V  H   !   TSLAKOCONT%NSPECIES       H   !   TSLAKOCONT%MINT "   æ  H   !   TSLAKOCONT%CUTOFF #   .  H   !   TSLAKOCONT%TDATAOK !   v  ¤   !   TSLAKOCONT%TINIT      À      MsObjComment 