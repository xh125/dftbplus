  ºC  £   k820309    Ù          2021.9.0    ÙVe                                                                                                          
       E:\workfiles\Githubfiles\dftbplus\build\src\dftbp\md\thermostat.f90 DFTBP_MD_THERMOSTAT              TTHERMOSTAT gen@INIT gen@GETINITVELOCITIES gen@UPDATEVELOCITIES gen@STATE                                                     
       DP                      @                              
       TANDERSENTHERMOSTAT GETINITVELOCITIES UPDATEVELOCITIES STATE                      @                              
       TBERENDSENTHERMOSTAT STATE UPDATEVELOCITIES GETINITVELOCITIES                      @                              
       TDUMMYTHERMOSTAT GETINITVELOCITIES INIT                      @                              
       TNHCTHERMOSTAT STATE UPDATEVELOCITIES GETINITVELOCITIES INIT                                                       u #DUMMYTHERMOSTAT_INIT    #MDCOMMON_INIT    #NHC_INIT    #THERMOSTAT_INIT_DUMMY $   #THERMOSTAT_INIT_ANDERSEN (   #THERMOSTAT_INIT_BERENDSEN ,   #THERMOSTAT_INIT_NHC 0   #         @     @                                                #THIS    #KT 	   #MASS 
   #PRANLUX    #PMDFRAME                                                    p               #TDUMMYTHERMOSTAT              
                                 	     
                
                                
                   
              &                                                    
                                      è               #TRANLUX              
                                                     #TMDCOMMON    #         @     @                                                #SF    #NMOVEDATOM    #NALLATOM    #TSTATIONARY                                                                   #TMDCOMMON              
                                                       
                                                                                                    #         @     @                                                #THIS    #PRANLUX    #MASSES    #TEMPPROFILE    #COUPLINGPARAMETER    #PMDFRAME    #DELTAT    #NPART    #NYS    #NC     #XNOSE !   #VNOSE "   #GNOSE #                                                   °              #TNHCTHERMOSTAT             
                                      è               #TRANLUX              
                                                   
              &                                                     
                                      ø              #TTEMPPROFILE              
                                      
                
                                                     #TMDCOMMON              
                                      
                
                                                       
                                                       
                                                        
                                !                   
              &                                                     
                                "                   
              &                                                     
                                #                   
 	             &                                           #         @     @X                             $                    #THIS %   #PTHERMOSTAT '             D                                 %     (               #TTHERMOSTAT &            
 @                               '     p               #TDUMMYTHERMOSTAT    #         @     @X                             (                    #THIS )   #PTHERMOSTAT *             D                                 )     (               #TTHERMOSTAT &            
 @                               *                    #TANDERSENTHERMOSTAT +   #         @     @X                             ,                    #THIS -   #PTHERMOSTAT .             D                                 -     (               #TTHERMOSTAT &            
 @                               .     x               #TBERENDSENTHERMOSTAT /   #         @     @X                             0                    #THIS 1   #PTHERMOSTAT 2             D                                 1     (               #TTHERMOSTAT &            
 @                               2     °              #TNHCTHERMOSTAT                                                         u #DUMMYTHERMOSTAT_GETINITVELOS 3   #NHC_GETINITVELOS 6   #THERMOSTAT_GETINITVELOCITIES 9   #         @     @                           3                    #THIS 4   #VELOCITIES 5             
                                 4     p               #TDUMMYTHERMOSTAT                                              5                   
               &                   &                                           #         @     @                           6                    #THIS 7   #VELOCITIES 8             
                                 7     °              #TNHCTHERMOSTAT                                              8                   
 
              &                   &                                           #         @     @X                             9                    #THIS :   #VELOCITIES ;             
D @                               :     (               #TTHERMOSTAT &             D @                              ;                   
               &                   &                                                                                                u #BERENDSEN_UPDATEVELOS <   #NHC_UPDATEVELOS ?   #THERMOSTAT_UPDATEVELOCITIES B   #         @     @                           <                    #THIS =   #VELOCITIES >             
                                 =     x               #TBERENDSENTHERMOSTAT /             
                                >                   
               &                   &                                           #         @     @                           ?                    #THIS @   #VELOCITIES A             
                                 @     °              #TNHCTHERMOSTAT              
                                A                   
               &                   &                                           #         @     @X                             B                    #THIS C   #VELOCITIES D             
D @                               C     (               #TTHERMOSTAT &             
D @                              D                   
               &                   &                                                                                                u #BERENDSEN_STATE E   #NHC_STATE H   #THERMOSTAT_STATE K   #         @     @                           E                    #THIS F   #FD G             
                                  F     x              #TBERENDSENTHERMOSTAT /             
                                  G           #         @     @                           H                    #THIS I   #FD J             
                                  I     °             #TNHCTHERMOSTAT              
                                  J           #         @     @X                             K                    #THIS L   #FD M             
  @                               L     (              #TTHERMOSTAT &             
  @                               M                                                                 u #ANDERSENTHERMOSTAT_GETINITVELOS N   #BERENDSEN_GETINITVELOS Q   #         @     @                           N                    #THIS O   #VELOCITIES P             
                                 O                    #TANDERSENTHERMOSTAT +                                             P                   
               &                   &                                           #         @     @                           Q                    #THIS R   #VELOCITIES S             
                                 R     x               #TBERENDSENTHERMOSTAT /                                             S                   
               &                   &                                                                                                 u #ANDERSENTHERMOSTAT_UPDATEVELOS T   #         @     @                           T                    #THIS U   #VELOCITIES V             
                                 U                    #TANDERSENTHERMOSTAT +             
                                V                   
               &                   &                                                                                                 u #ANDERSENTHERMOSTAT_STATE W   #         @     @                           W                    #THIS X   #FD Y             
                                  X                   #TANDERSENTHERMOSTAT +             
                                  Y                             @              @           +     '                    #NATOM Z   #PRANLUX [   #MASS f   #PTEMPPROFILE g   #TRESCALEINDIV w   #WVSCALE x   #PMDFRAMEWORK y                 D                              Z                                D                              [     è                     #TRANLUX                   @  @                              'è              
      #NEXT \   #LUXLEV ]   #NSKIP ^   #IN24 _   #I24 `   #J24 a   #ISEEDS b   #ICARRY c   #TWOM24 d   #TWOM12 e                 $                              \                                p          p            p                                        $                              ]     `                           $                              ^     d                           $                              _     h                           $                              `     l                           $                              a     p                           $                              b            t                   p          p            p                                        $                              c     Ô                           $                             d     Ø       	   
                 $                             e     à       
   
               D                             f                             
            &                                                        D                              g     ø       X              #TTEMPPROFILE                  À  @              D                'ø              
      #TEMPMETHODS h   #TEMPINTS i   #TEMPVALUES j   #CURTEMP k   #IINT l   #NINT m   #ISTEP n   #INCR o   #NEXT p   #GETTEMPERATURE s               D                             h                                          &                                                       D                             i            H                             &                                                       D                            j                             
            &                                                         D                            k     Ø          
                 D                             l     à                           D                             m     ä                           D                             n     è                           D                            o     ð          
   1         À    $                            p             	     #NEXT q   #         @     @                            q                    #THIS r             
                                r     ø               #TTEMPPROFILE    1         À    $                            s             
     #GETTEMPERATURE t   #         @     @                            t                    #THIS u   #TEMP v             
                                 u     ø              #TTEMPPROFILE                                              v     
                     D                              w     `                           D                             x     h          
                 D                              y            p              #TMDCOMMON                   @  @                              '                    #NF z   #TSTATIONARY {                 $                             z                
                 $                              {                                    @              @           /     'x                    #NATOM |   #PRANLUX }   #MASS ~   #PTEMPPROFILE    #COUPLINGPARAMETER    #PMDFRAME                  D                              |                                D                              }     è                     #TRANLUX                D                             ~                             
            &                                                        D                                   ø       X              #TTEMPPROFILE                  D                                  `          
                 D                                          h              #TMDCOMMON                      @              @                'p                    #NATOM    #KT    #MASS    #PRANLUX    #PMDFRAME                  D                                                               D                                            
               D                                                          
            &                                                        D                                   è       X              #TRANLUX                  D                                          `              #TMDCOMMON                      @              @                '°                   #NATOM    #PRANLUX    #MASS    #PTEMPPROFILE    #COUPLINGPARAMETER    #PMDFRAME    #DELTAT    #NYOSH    #W    #NRESN    #NNOS    #XNOSE    #VNOSE    #GNOSE                  D                                                              D                                   è                     #TRANLUX                D                                                          
            &                                                        D                                   ø       X              #TTEMPPROFILE                  D                                  `          
                 D                                          h              #TMDCOMMON                  D                                  x          
                 D                                                            D                                                       	   
            &                                                         D                                   Ð       
                    D                                   Ô                         D                                         Ø                 
            &                                                       D                                                          
            &                                                       D                                         h                
            &                                                             @               À           &     '(                    #THERMOSTAT    #PDUMMY    #PANDERSEN    #PBERENDSEN    #PNHC                  D                                                              D                                   p                     #TDUMMYTHERMOSTAT                 D                                                        #TANDERSENTHERMOSTAT +                D                                   x                     #TBERENDSENTHERMOSTAT /                D                                   °                     #TNHCTHERMOSTAT    *          n                         º              Cifmodintr.lib   n                           º              Cifmodintr.lib                                               `      fn#fn )      Z   b   uapp(DFTBP_MD_THERMOSTAT &   Z  C   J  DFTBP_COMMON_ACCURACY '     }   J  DFTBP_MD_ANDERSENTHERM (     ~   J  DFTBP_MD_BERENDSENTHERM $     h   J  DFTBP_MD_DUMMYTHERM "      }   J  DFTBP_MD_NHCTHERM    }  ì       gen@INIT 9   i        DUMMYTHERMOSTAT_INIT+DFTBP_MD_DUMMYTHERM >   è  ^   a   DUMMYTHERMOSTAT_INIT%THIS+DFTBP_MD_DUMMYTHERM <   F  @   a   DUMMYTHERMOSTAT_INIT%KT+DFTBP_MD_DUMMYTHERM >        a   DUMMYTHERMOSTAT_INIT%MASS+DFTBP_MD_DUMMYTHERM A     U   a   DUMMYTHERMOSTAT_INIT%PRANLUX+DFTBP_MD_DUMMYTHERM B   g  W   a   DUMMYTHERMOSTAT_INIT%PMDFRAME+DFTBP_MD_DUMMYTHERM 0   ¾        MDCOMMON_INIT+DFTBP_MD_MDCOMMON 3   =  W   a   MDCOMMON_INIT%SF+DFTBP_MD_MDCOMMON ;     @   a   MDCOMMON_INIT%NMOVEDATOM+DFTBP_MD_MDCOMMON 9   Ô  @   a   MDCOMMON_INIT%NALLATOM+DFTBP_MD_MDCOMMON <     @   a   MDCOMMON_INIT%TSTATIONARY+DFTBP_MD_MDCOMMON +   T  ê      NHC_INIT+DFTBP_MD_NHCTHERM 0   >	  \   a   NHC_INIT%THIS+DFTBP_MD_NHCTHERM 3   	  U   a   NHC_INIT%PRANLUX+DFTBP_MD_NHCTHERM 2   ï	     a   NHC_INIT%MASSES+DFTBP_MD_NHCTHERM 7   {
  Z   a   NHC_INIT%TEMPPROFILE+DFTBP_MD_NHCTHERM =   Õ
  @   a   NHC_INIT%COUPLINGPARAMETER+DFTBP_MD_NHCTHERM 4     W   a   NHC_INIT%PMDFRAME+DFTBP_MD_NHCTHERM 2   l  @   a   NHC_INIT%DELTAT+DFTBP_MD_NHCTHERM 1   ¬  @   a   NHC_INIT%NPART+DFTBP_MD_NHCTHERM /   ì  @   a   NHC_INIT%NYS+DFTBP_MD_NHCTHERM .   ,  @   a   NHC_INIT%NC+DFTBP_MD_NHCTHERM 1   l     a   NHC_INIT%XNOSE+DFTBP_MD_NHCTHERM 1   ø     a   NHC_INIT%VNOSE+DFTBP_MD_NHCTHERM 1        a   NHC_INIT%GNOSE+DFTBP_MD_NHCTHERM &     c      THERMOSTAT_INIT_DUMMY +   s  Y   a   THERMOSTAT_INIT_DUMMY%THIS 2   Ì  ^   a   THERMOSTAT_INIT_DUMMY%PTHERMOSTAT )   *  c      THERMOSTAT_INIT_ANDERSEN .     Y   a   THERMOSTAT_INIT_ANDERSEN%THIS 5   æ  a   a   THERMOSTAT_INIT_ANDERSEN%PTHERMOSTAT *   G  c      THERMOSTAT_INIT_BERENDSEN /   ª  Y   a   THERMOSTAT_INIT_BERENDSEN%THIS 6     b   a   THERMOSTAT_INIT_BERENDSEN%PTHERMOSTAT $   e  c      THERMOSTAT_INIT_NHC )   È  Y   a   THERMOSTAT_INIT_NHC%THIS 0   !  \   a   THERMOSTAT_INIT_NHC%PTHERMOSTAT &   }         gen@GETINITVELOCITIES A     b      DUMMYTHERMOSTAT_GETINITVELOS+DFTBP_MD_DUMMYTHERM F   y  ^   a   DUMMYTHERMOSTAT_GETINITVELOS%THIS+DFTBP_MD_DUMMYTHERM L   ×  ¤   a   DUMMYTHERMOSTAT_GETINITVELOS%VELOCITIES+DFTBP_MD_DUMMYTHERM 3   {  b      NHC_GETINITVELOS+DFTBP_MD_NHCTHERM 8   Ý  \   a   NHC_GETINITVELOS%THIS+DFTBP_MD_NHCTHERM >   9  ¤   a   NHC_GETINITVELOS%VELOCITIES+DFTBP_MD_NHCTHERM -   Ý  b      THERMOSTAT_GETINITVELOCITIES 2   ?  Y   a   THERMOSTAT_GETINITVELOCITIES%THIS 8     ¤   a   THERMOSTAT_GETINITVELOCITIES%VELOCITIES %   <         gen@UPDATEVELOCITIES >   Í  b      BERENDSEN_UPDATEVELOS+DFTBP_MD_BERENDSENTHERM C   /  b   a   BERENDSEN_UPDATEVELOS%THIS+DFTBP_MD_BERENDSENTHERM I     ¤   a   BERENDSEN_UPDATEVELOS%VELOCITIES+DFTBP_MD_BERENDSENTHERM 2   5  b      NHC_UPDATEVELOS+DFTBP_MD_NHCTHERM 7     \   a   NHC_UPDATEVELOS%THIS+DFTBP_MD_NHCTHERM =   ó  ¤   a   NHC_UPDATEVELOS%VELOCITIES+DFTBP_MD_NHCTHERM ,     b      THERMOSTAT_UPDATEVELOCITIES 1   ù  Y   a   THERMOSTAT_UPDATEVELOCITIES%THIS 7   R  ¤   a   THERMOSTAT_UPDATEVELOCITIES%VELOCITIES    ö  z       gen@STATE 8   p  Z      BERENDSEN_STATE+DFTBP_MD_BERENDSENTHERM =   Ê  b   a   BERENDSEN_STATE%THIS+DFTBP_MD_BERENDSENTHERM ;   ,  @   a   BERENDSEN_STATE%FD+DFTBP_MD_BERENDSENTHERM ,   l  Z      NHC_STATE+DFTBP_MD_NHCTHERM 1   Æ  \   a   NHC_STATE%THIS+DFTBP_MD_NHCTHERM /   "  @   a   NHC_STATE%FD+DFTBP_MD_NHCTHERM !   b  Z      THERMOSTAT_STATE &   ¼  Y   a   THERMOSTAT_STATE%THIS $     @   a   THERMOSTAT_STATE%FD =   U         gen@GETINITVELOCITIES+DFTBP_MD_ANDERSENTHERM G   Ö  b      ANDERSENTHERMOSTAT_GETINITVELOS+DFTBP_MD_ANDERSENTHERM L   8   a   a   ANDERSENTHERMOSTAT_GETINITVELOS%THIS+DFTBP_MD_ANDERSENTHERM R      ¤   a   ANDERSENTHERMOSTAT_GETINITVELOS%VELOCITIES+DFTBP_MD_ANDERSENTHERM ?   =!  b      BERENDSEN_GETINITVELOS+DFTBP_MD_BERENDSENTHERM D   !  b   a   BERENDSEN_GETINITVELOS%THIS+DFTBP_MD_BERENDSENTHERM J   "  ¤   a   BERENDSEN_GETINITVELOS%VELOCITIES+DFTBP_MD_BERENDSENTHERM <   ¥"  d       gen@UPDATEVELOCITIES+DFTBP_MD_ANDERSENTHERM F   	#  b      ANDERSENTHERMOSTAT_UPDATEVELOS+DFTBP_MD_ANDERSENTHERM K   k#  a   a   ANDERSENTHERMOSTAT_UPDATEVELOS%THIS+DFTBP_MD_ANDERSENTHERM Q   Ì#  ¤   a   ANDERSENTHERMOSTAT_UPDATEVELOS%VELOCITIES+DFTBP_MD_ANDERSENTHERM 1   p$  ^       gen@STATE+DFTBP_MD_ANDERSENTHERM @   Î$  Z      ANDERSENTHERMOSTAT_STATE+DFTBP_MD_ANDERSENTHERM E   (%  a   a   ANDERSENTHERMOSTAT_STATE%THIS+DFTBP_MD_ANDERSENTHERM C   %  @   a   ANDERSENTHERMOSTAT_STATE%FD+DFTBP_MD_ANDERSENTHERM ;   É%  ¶       TANDERSENTHERMOSTAT+DFTBP_MD_ANDERSENTHERM G   &  H   %   TANDERSENTHERMOSTAT%NATOM+DFTBP_MD_ANDERSENTHERM=NATOM K   Ç&  ]   %   TANDERSENTHERMOSTAT%PRANLUX+DFTBP_MD_ANDERSENTHERM=PRANLUX *   $'  ½      TRANLUX+DFTBP_MATH_RANLUX /   á'     a   TRANLUX%NEXT+DFTBP_MATH_RANLUX 1   }(  H   a   TRANLUX%LUXLEV+DFTBP_MATH_RANLUX 0   Å(  H   a   TRANLUX%NSKIP+DFTBP_MATH_RANLUX /   )  H   a   TRANLUX%IN24+DFTBP_MATH_RANLUX .   U)  H   a   TRANLUX%I24+DFTBP_MATH_RANLUX .   )  H   a   TRANLUX%J24+DFTBP_MATH_RANLUX 1   å)     a   TRANLUX%ISEEDS+DFTBP_MATH_RANLUX 1   *  H   a   TRANLUX%ICARRY+DFTBP_MATH_RANLUX 1   É*  H   a   TRANLUX%TWOM24+DFTBP_MATH_RANLUX 1   +  H   a   TRANLUX%TWOM12+DFTBP_MATH_RANLUX E   Y+     %   TANDERSENTHERMOSTAT%MASS+DFTBP_MD_ANDERSENTHERM=MASS U   í+  b   %   TANDERSENTHERMOSTAT%PTEMPPROFILE+DFTBP_MD_ANDERSENTHERM=PTEMPPROFILE 2   O,  Ó      TTEMPPROFILE+DFTBP_MD_TEMPPROFILE J   "-     %   TTEMPPROFILE%TEMPMETHODS+DFTBP_MD_TEMPPROFILE=TEMPMETHODS D   ¶-     %   TTEMPPROFILE%TEMPINTS+DFTBP_MD_TEMPPROFILE=TEMPINTS H   J.     %   TTEMPPROFILE%TEMPVALUES+DFTBP_MD_TEMPPROFILE=TEMPVALUES B   Þ.  H   %   TTEMPPROFILE%CURTEMP+DFTBP_MD_TEMPPROFILE=CURTEMP <   &/  H   %   TTEMPPROFILE%IINT+DFTBP_MD_TEMPPROFILE=IINT <   n/  H   %   TTEMPPROFILE%NINT+DFTBP_MD_TEMPPROFILE=NINT >   ¶/  H   %   TTEMPPROFILE%ISTEP+DFTBP_MD_TEMPPROFILE=ISTEP <   þ/  H   %   TTEMPPROFILE%INCR+DFTBP_MD_TEMPPROFILE=INCR 7   F0  R   a   TTEMPPROFILE%NEXT+DFTBP_MD_TEMPPROFILE *   0  R      NEXT+DFTBP_MD_TEMPPROFILE /   ê0  Z   a   NEXT%THIS+DFTBP_MD_TEMPPROFILE A   D1  \   a   TTEMPPROFILE%GETTEMPERATURE+DFTBP_MD_TEMPPROFILE 4    1  \      GETTEMPERATURE+DFTBP_MD_TEMPPROFILE 9   ü1  Z   a   GETTEMPERATURE%THIS+DFTBP_MD_TEMPPROFILE 9   V2  @   a   GETTEMPERATURE%TEMP+DFTBP_MD_TEMPPROFILE W   2  H   %   TANDERSENTHERMOSTAT%TRESCALEINDIV+DFTBP_MD_ANDERSENTHERM=TRESCALEINDIV K   Þ2  H   %   TANDERSENTHERMOSTAT%WVSCALE+DFTBP_MD_ANDERSENTHERM=WVSCALE U   &3  _   %   TANDERSENTHERMOSTAT%PMDFRAMEWORK+DFTBP_MD_ANDERSENTHERM=PMDFRAMEWORK ,   3  i      TMDCOMMON+DFTBP_MD_MDCOMMON /   î3  H   a   TMDCOMMON%NF+DFTBP_MD_MDCOMMON 8   64  H   a   TMDCOMMON%TSTATIONARY+DFTBP_MD_MDCOMMON =   ~4  ©       TBERENDSENTHERMOSTAT+DFTBP_MD_BERENDSENTHERM I   '5  H   %   TBERENDSENTHERMOSTAT%NATOM+DFTBP_MD_BERENDSENTHERM=NATOM M   o5  ]   %   TBERENDSENTHERMOSTAT%PRANLUX+DFTBP_MD_BERENDSENTHERM=PRANLUX G   Ì5     %   TBERENDSENTHERMOSTAT%MASS+DFTBP_MD_BERENDSENTHERM=MASS W   `6  b   %   TBERENDSENTHERMOSTAT%PTEMPPROFILE+DFTBP_MD_BERENDSENTHERM=PTEMPPROFILE a   Â6  H   %   TBERENDSENTHERMOSTAT%COUPLINGPARAMETER+DFTBP_MD_BERENDSENTHERM=COUPLINGPARAMETER O   
7  _   %   TBERENDSENTHERMOSTAT%PMDFRAME+DFTBP_MD_BERENDSENTHERM=PMDFRAME 5   i7         TDUMMYTHERMOSTAT+DFTBP_MD_DUMMYTHERM A   ñ7  H   %   TDUMMYTHERMOSTAT%NATOM+DFTBP_MD_DUMMYTHERM=NATOM ;   98  H   %   TDUMMYTHERMOSTAT%KT+DFTBP_MD_DUMMYTHERM=KT ?   8     %   TDUMMYTHERMOSTAT%MASS+DFTBP_MD_DUMMYTHERM=MASS E   9  ]   %   TDUMMYTHERMOSTAT%PRANLUX+DFTBP_MD_DUMMYTHERM=PRANLUX G   r9  _   %   TDUMMYTHERMOSTAT%PMDFRAME+DFTBP_MD_DUMMYTHERM=PMDFRAME 1   Ñ9  ý       TNHCTHERMOSTAT+DFTBP_MD_NHCTHERM =   Î:  H   %   TNHCTHERMOSTAT%NATOM+DFTBP_MD_NHCTHERM=NATOM A   ;  ]   %   TNHCTHERMOSTAT%PRANLUX+DFTBP_MD_NHCTHERM=PRANLUX ;   s;     %   TNHCTHERMOSTAT%MASS+DFTBP_MD_NHCTHERM=MASS K   <  b   %   TNHCTHERMOSTAT%PTEMPPROFILE+DFTBP_MD_NHCTHERM=PTEMPPROFILE U   i<  H   %   TNHCTHERMOSTAT%COUPLINGPARAMETER+DFTBP_MD_NHCTHERM=COUPLINGPARAMETER C   ±<  _   %   TNHCTHERMOSTAT%PMDFRAME+DFTBP_MD_NHCTHERM=PMDFRAME ?   =  H   %   TNHCTHERMOSTAT%DELTAT+DFTBP_MD_NHCTHERM=DELTAT =   X=  H   %   TNHCTHERMOSTAT%NYOSH+DFTBP_MD_NHCTHERM=NYOSH 5    =     %   TNHCTHERMOSTAT%W+DFTBP_MD_NHCTHERM=W =   4>  H   %   TNHCTHERMOSTAT%NRESN+DFTBP_MD_NHCTHERM=NRESN ;   |>  H   %   TNHCTHERMOSTAT%NNOS+DFTBP_MD_NHCTHERM=NNOS =   Ä>     %   TNHCTHERMOSTAT%XNOSE+DFTBP_MD_NHCTHERM=XNOSE =   X?     %   TNHCTHERMOSTAT%VNOSE+DFTBP_MD_NHCTHERM=VNOSE =   ì?     %   TNHCTHERMOSTAT%GNOSE+DFTBP_MD_NHCTHERM=GNOSE    @         TTHERMOSTAT '   A  H   !   TTHERMOSTAT%THERMOSTAT #   ]A  f   !   TTHERMOSTAT%PDUMMY &   ÃA  i   !   TTHERMOSTAT%PANDERSEN '   ,B  j   !   TTHERMOSTAT%PBERENDSEN !   B  d   !   TTHERMOSTAT%PNHC    úB  À      MsObjComment 