  ­D     k820309    Ù          2021.9.0    ÙVe                                                                                                          
       E:\workfiles\Githubfiles\dftbplus\build\src\dftbp\dftb\halogenx.f90 DFTBP_DFTB_HALOGENX              THALOGENX THALOGENX_INIT HALOGENXSPECIES1 HALOGENXSPECIES2                                                     
       DP MC                      @                              
       AA__BOHR BOHR__AA KCAL_MOL__HARTREE                      @                              
       TNEIGHBOURLIST GETNROFNEIGHBOURSFORALL                      @                              
       GETVDWDATA                      @                              
       ERROR               @                                      u #ERROR_SINGLE    #ERROR_ARRAY    #         @     @                                               #MESSAGE              
                                                    1 #         @     @                                                #MESSAGES 	   ,          
                               	                                  &                                           1               @  @               A          
     'À                   #NNEIGHBOUR    #INEIGHBOUR    #NEIGHDIST2    #CUTOFF    #INITIALIZED    #SETEXTERNALLY    #USEMPIWINDOWS_    #INEIGHBOURMEM_    #NEIGHDIST2MEM_    #INEIGHBOURWIN_    #NEIGHDIST2WIN_ M   #TNEIGHBOURLIST_FINAL N              $                                                                        &                                                      $                                          H                            &                   &                                                                                  y                                                          $                                         ¨                
            &                   &                                                                                  y
                                                             $                                           
                $                                                                                                                                                 $                                                                                                                                                 D                                                                                                                                               D                                                                      &                                                                                  y                                                          D                                         h             	  
            &                                                                                  y
                                                             D                                          °      
       #MPIFX_WIN                  À  @                              '                    #ID    #COMM_ID    #ALLOCATE_SHARED    #MPIFX_WIN_ALLOCATE_SHARED_I    #MPIFX_WIN_ALLOCATE_SHARED_S    #MPIFX_WIN_ALLOCATE_SHARED_D    #MPIFX_WIN_ALLOCATE_SHARED_C    #MPIFX_WIN_ALLOCATE_SHARED_Z    #LOCK =   #UNLOCK A   #SYNC E   #FREE I                 $                                                               D                                               4             $                         @                        3             $                         @             u #MPIFX_WIN    #MPIFX_WIN_ALLOCATE_SHARED_I    #MPIFX_WIN_ALLOCATE_SHARED_S    #MPIFX_WIN_ALLOCATE_SHARED_D    #MPIFX_WIN_ALLOCATE_SHARED_C    #MPIFX_WIN_ALLOCATE_SHARED_Z    1         À    D                                             #MPIFX_WIN_ALLOCATE_SHARED_I    #         @     @                                                #SELF    #MYCOMM     #LENGTH "   #SHARED_DATA #   #ERROR $                                                                 #MPIFX_WIN              
                                                     #MPIFX_COMM !             
                                  "                                                    #                                  &                                                                                      $            1         À    D                                             #MPIFX_WIN_ALLOCATE_SHARED_S %   #         @     @                            %                    #SELF &   #MYCOMM '   #LENGTH (   #SHARED_DATA )   #ERROR *                                             &                    #MPIFX_WIN              
                                 '                   #MPIFX_COMM !             
                                  (                                                   )                   	               &                                                                                      *            1         À    D                                             #MPIFX_WIN_ALLOCATE_SHARED_D +   #         @     @                            +                    #SELF ,   #MYCOMM -   #LENGTH .   #SHARED_DATA /   #ERROR 0                                             ,                    #MPIFX_WIN              
                                 -                   #MPIFX_COMM !             
                                  .                                                   /                   
               &                                                                                      0            1         À    D                                             #MPIFX_WIN_ALLOCATE_SHARED_C 1   #         @     @                            1                    #SELF 2   #MYCOMM 3   #LENGTH 4   #SHARED_DATA 5   #ERROR 6                                             2                    #MPIFX_WIN              
                                 3                   #MPIFX_COMM !             
                                  4                                                   5                                  &                                                                                      6            1         À    D                                             #MPIFX_WIN_ALLOCATE_SHARED_Z 7   #         @     @                            7                    #SELF 8   #MYCOMM 9   #LENGTH :   #SHARED_DATA ;   #ERROR <                                             8                    #MPIFX_WIN              
                                 9                   #MPIFX_COMM !             
                                  :                                                   ;                                  &                                                                                      <            1         À    $                            =             	     #MPIFX_WIN_LOCK >   #         @     @                            >                    #SELF ?   #ERROR @             
                                ?                    #MPIFX_WIN                                               @            1         À    $                            A             
     #MPIFX_WIN_UNLOCK B   #         @     @                            B                    #SELF C   #ERROR D             
                                C                    #MPIFX_WIN                                               D            1         À    $                            E                  #MPIFX_WIN_SYNC F   #         @     @                            F                    #SELF G   #ERROR H             
                                G                    #MPIFX_WIN                                               H            1         À    $                            I              	    #MPIFX_WIN_FREE J   #         @     @                            J                    #SELF K   #ERROR L             
                                K                    #MPIFX_WIN                                               L                          D                              M            ¸             #MPIFX_WIN    2         À                                 N             #TNEIGHBOURLIST_FINAL O   #         @     @                           O                    #THIS P             
                                 P     À              #TNEIGHBOURLIST 
                 @  @                         !     '              	      #ID Q   #SIZE R   #RANK S   #LEADRANK T   #LEAD U   #INIT V   #SPLIT [   #SPLIT_TYPE b   #FREE i                 $                              Q                                 $                              R                                $                              S                                $                              T                                $                              U                  1         À    $                            V                  #MPIFX_COMM_INIT W   #         @     @                            W                    #SELF X   #COMMID Y   #ERROR Z                                             X                    #MPIFX_COMM !             
                                 Y                                                      Z            1         À    $                            [                  #MPIFX_COMM_SPLIT \   #         @     @                            \                    #SELF ]   #SPLITKEY ^   #RANKKEY _   #NEWCOMM `   #ERROR a             
                                ]                    #MPIFX_COMM !             
                                  ^                     
                                  _                                                     `                    #MPIFX_COMM !                                              a            1         À    $                            b                  #MPIFX_COMM_SPLIT_TYPE c   #         @     @                            c                    #SELF d   #SPLITTYPE e   #RANKKEY f   #NEWCOMM g   #ERROR h             
                                d                    #MPIFX_COMM !             
                                  e                     
                                  f                                                     g                    #MPIFX_COMM !                                              h            1         À    $                            i             	     #MPIFX_COMM_FREE j   #         @     @                            j                    #SELF k             
                                k                    #MPIFX_COMM !                     @               A           l     'À              	      #INTERACTIONTYPE m   #RADII n   #MAXDAB o   #NATOM p   #CUTOFF q   #GETRCUTOFF r   #GETENERGIES u   #ADDGRADIENTS }   #GETSTRESS                D                              m                                          &                   &                                                       D                             n            `                 
            &                                                        D                             o     ¨         
                                                
                                 0.0                  D                              p     °                          D                             q     ¸         
                                                
                                 0.0    1         À    $                           r                  #GETRCUTOFF s   %         @   @                            s                    
       #THIS t             
                                t     À               #THALOGENX l   1         À    $                            u                  #GETENERGIES v   #         @     @                             v                    #THIS w   #ATOME x   #COORDS y   #SPECIES z   #NEIGH {   #IMG2CENTCELL |             
                                 w     À              #THALOGENX l             D                                x                   
 	              &                                                     
                                 y                   
 
             &                   &                                                     
                                  z                                 &                                                     
  @                               {     À             #TNEIGHBOURLIST 
             
                                  |                                 &                                           1         À    $                            }                  #ADDGRADIENTS ~   #         @     @                             ~                    #THIS    #DERIVS    #COORDS    #SPECIES    #NEIGH    #IMG2CENTCELL              
                                      À              #THALOGENX l             
D                                                   
               &                   &                                                     
                                                    
              &                   &                                                     
                                                                   &                                                     
  @                                    À             #TNEIGHBOURLIST 
             
                                                                   &                                           1         À    $                                         	     #GETSTRESS    #         @     @                                                 #THIS    #ST    #COORDS    #NEIGH    #SPECIES    #IMG2CENTCELL    #CELLVOL              
                                      À              #THALOGENX l             D                                                   
               &                   &                                                     
                                                    
              &                   &                                                     
  @                                    À             #TNEIGHBOURLIST 
             
                                                                   &                                                     
                                                                   &                                                     
                                      
      #         @                                                       #THIS    #SPECIES0    #SPECIESNAMES              D @                                    À               #THALOGENX l             
 @                                                                &                                           ,          
                                                                  &                                           1             @                                                                                                    TWp          n                                         CO  n                                           CN  h  p          p          p            p                                                                                            @                                                                                                    TWp          n                                         CCl  n                                          CBr  n                                          CI   h  p          p          p            p                                                                                                    *          n                                       Cifmodintr.lib   n                                         Cifmodintr.lib                                               `      fn#fn )      K   b   uapp(DFTBP_DFTB_HALOGENX &   K  F   J  DFTBP_COMMON_ACCURACY '     d   J  DFTBP_COMMON_CONSTANTS $   õ  g   J  DFTBP_DFTB_PERIODIC #   \  K   J  DFTBP_DFTB_VDWDATA !   §  F   J  DFTBP_IO_MESSAGE +   í  c       gen@ERROR+DFTBP_IO_MESSAGE .   P  U      ERROR_SINGLE+DFTBP_IO_MESSAGE 6   ¥  L   a   ERROR_SINGLE%MESSAGE+DFTBP_IO_MESSAGE -   ñ  V      ERROR_ARRAY+DFTBP_IO_MESSAGE 6   G     a   ERROR_ARRAY%MESSAGES+DFTBP_IO_MESSAGE 3   ×  .     TNEIGHBOURLIST+DFTBP_DFTB_PERIODIC >        a   TNEIGHBOURLIST%NNEIGHBOUR+DFTBP_DFTB_PERIODIC >       a   TNEIGHBOURLIST%INEIGHBOUR+DFTBP_DFTB_PERIODIC >   ¥    a   TNEIGHBOURLIST%NEIGHDIST2+DFTBP_DFTB_PERIODIC :   ±  H   a   TNEIGHBOURLIST%CUTOFF+DFTBP_DFTB_PERIODIC ?   ù  ¤   a   TNEIGHBOURLIST%INITIALIZED+DFTBP_DFTB_PERIODIC A   	  ¤   a   TNEIGHBOURLIST%SETEXTERNALLY+DFTBP_DFTB_PERIODIC Q   A
  ¤   %   TNEIGHBOURLIST%USEMPIWINDOWS_+DFTBP_DFTB_PERIODIC=USEMPIWINDOWS_ Q   å
  ô   %   TNEIGHBOURLIST%INEIGHBOURMEM_+DFTBP_DFTB_PERIODIC=INEIGHBOURMEM_ Q   Ù  ô   %   TNEIGHBOURLIST%NEIGHDIST2MEM_+DFTBP_DFTB_PERIODIC=NEIGHDIST2MEM_ Q   Í  _   %   TNEIGHBOURLIST%INEIGHBOURWIN_+DFTBP_DFTB_PERIODIC=INEIGHBOURWIN_ +   ,  I     MPIFX_WIN+MPIFX_WIN_MODULE .   u  H   a   MPIFX_WIN%ID+MPIFX_WIN_MODULE ;   ½  H   %   MPIFX_WIN%COMM_ID+MPIFX_WIN_MODULE=COMM_ID ;     H   a   MPIFX_WIN%ALLOCATE_SHARED+MPIFX_WIN_MODULE 8   M  ô   `   gen@ALLOCATE_SHARED+DFTBP_DFTB_PERIODIC c   A  i   %   MPIFX_WIN%MPIFX_WIN_ALLOCATE_SHARED_I+MPIFX_WIN_MODULE=MPIFX_WIN_ALLOCATE_SHARED_I =   ª        MPIFX_WIN_ALLOCATE_SHARED_I+MPIFX_WIN_MODULE B   0  W   a   MPIFX_WIN_ALLOCATE_SHARED_I%SELF+MPIFX_WIN_MODULE D     X   a   MPIFX_WIN_ALLOCATE_SHARED_I%MYCOMM+MPIFX_WIN_MODULE D   ß  @   a   MPIFX_WIN_ALLOCATE_SHARED_I%LENGTH+MPIFX_WIN_MODULE I        a   MPIFX_WIN_ALLOCATE_SHARED_I%SHARED_DATA+MPIFX_WIN_MODULE C   «  @   a   MPIFX_WIN_ALLOCATE_SHARED_I%ERROR+MPIFX_WIN_MODULE c   ë  i   %   MPIFX_WIN%MPIFX_WIN_ALLOCATE_SHARED_S+MPIFX_WIN_MODULE=MPIFX_WIN_ALLOCATE_SHARED_S =   T        MPIFX_WIN_ALLOCATE_SHARED_S+MPIFX_WIN_MODULE B   Ú  W   a   MPIFX_WIN_ALLOCATE_SHARED_S%SELF+MPIFX_WIN_MODULE D   1  X   a   MPIFX_WIN_ALLOCATE_SHARED_S%MYCOMM+MPIFX_WIN_MODULE D     @   a   MPIFX_WIN_ALLOCATE_SHARED_S%LENGTH+MPIFX_WIN_MODULE I   É     a   MPIFX_WIN_ALLOCATE_SHARED_S%SHARED_DATA+MPIFX_WIN_MODULE C   U  @   a   MPIFX_WIN_ALLOCATE_SHARED_S%ERROR+MPIFX_WIN_MODULE c     i   %   MPIFX_WIN%MPIFX_WIN_ALLOCATE_SHARED_D+MPIFX_WIN_MODULE=MPIFX_WIN_ALLOCATE_SHARED_D =   þ        MPIFX_WIN_ALLOCATE_SHARED_D+MPIFX_WIN_MODULE B     W   a   MPIFX_WIN_ALLOCATE_SHARED_D%SELF+MPIFX_WIN_MODULE D   Û  X   a   MPIFX_WIN_ALLOCATE_SHARED_D%MYCOMM+MPIFX_WIN_MODULE D   3  @   a   MPIFX_WIN_ALLOCATE_SHARED_D%LENGTH+MPIFX_WIN_MODULE I   s     a   MPIFX_WIN_ALLOCATE_SHARED_D%SHARED_DATA+MPIFX_WIN_MODULE C   ÿ  @   a   MPIFX_WIN_ALLOCATE_SHARED_D%ERROR+MPIFX_WIN_MODULE c   ?  i   %   MPIFX_WIN%MPIFX_WIN_ALLOCATE_SHARED_C+MPIFX_WIN_MODULE=MPIFX_WIN_ALLOCATE_SHARED_C =   ¨        MPIFX_WIN_ALLOCATE_SHARED_C+MPIFX_WIN_MODULE B   .  W   a   MPIFX_WIN_ALLOCATE_SHARED_C%SELF+MPIFX_WIN_MODULE D     X   a   MPIFX_WIN_ALLOCATE_SHARED_C%MYCOMM+MPIFX_WIN_MODULE D   Ý  @   a   MPIFX_WIN_ALLOCATE_SHARED_C%LENGTH+MPIFX_WIN_MODULE I        a   MPIFX_WIN_ALLOCATE_SHARED_C%SHARED_DATA+MPIFX_WIN_MODULE C   ©  @   a   MPIFX_WIN_ALLOCATE_SHARED_C%ERROR+MPIFX_WIN_MODULE c   é  i   %   MPIFX_WIN%MPIFX_WIN_ALLOCATE_SHARED_Z+MPIFX_WIN_MODULE=MPIFX_WIN_ALLOCATE_SHARED_Z =   R        MPIFX_WIN_ALLOCATE_SHARED_Z+MPIFX_WIN_MODULE B   Ø  W   a   MPIFX_WIN_ALLOCATE_SHARED_Z%SELF+MPIFX_WIN_MODULE D   /  X   a   MPIFX_WIN_ALLOCATE_SHARED_Z%MYCOMM+MPIFX_WIN_MODULE D     @   a   MPIFX_WIN_ALLOCATE_SHARED_Z%LENGTH+MPIFX_WIN_MODULE I   Ç     a   MPIFX_WIN_ALLOCATE_SHARED_Z%SHARED_DATA+MPIFX_WIN_MODULE C   S  @   a   MPIFX_WIN_ALLOCATE_SHARED_Z%ERROR+MPIFX_WIN_MODULE 0     \   a   MPIFX_WIN%LOCK+MPIFX_WIN_MODULE 0   ï  ]      MPIFX_WIN_LOCK+MPIFX_WIN_MODULE 5   L  W   a   MPIFX_WIN_LOCK%SELF+MPIFX_WIN_MODULE 6   £  @   a   MPIFX_WIN_LOCK%ERROR+MPIFX_WIN_MODULE 2   ã  ^   a   MPIFX_WIN%UNLOCK+MPIFX_WIN_MODULE 2   A  ]      MPIFX_WIN_UNLOCK+MPIFX_WIN_MODULE 7     W   a   MPIFX_WIN_UNLOCK%SELF+MPIFX_WIN_MODULE 8   õ  @   a   MPIFX_WIN_UNLOCK%ERROR+MPIFX_WIN_MODULE 0   5   \   a   MPIFX_WIN%SYNC+MPIFX_WIN_MODULE 0      ]      MPIFX_WIN_SYNC+MPIFX_WIN_MODULE 5   î   W   a   MPIFX_WIN_SYNC%SELF+MPIFX_WIN_MODULE 6   E!  @   a   MPIFX_WIN_SYNC%ERROR+MPIFX_WIN_MODULE 0   !  \   a   MPIFX_WIN%FREE+MPIFX_WIN_MODULE 0   á!  ]      MPIFX_WIN_FREE+MPIFX_WIN_MODULE 5   >"  W   a   MPIFX_WIN_FREE%SELF+MPIFX_WIN_MODULE 6   "  @   a   MPIFX_WIN_FREE%ERROR+MPIFX_WIN_MODULE Q   Õ"  _   %   TNEIGHBOURLIST%NEIGHDIST2WIN_+DFTBP_DFTB_PERIODIC=NEIGHDIST2WIN_ H   4#  Z   a   TNEIGHBOURLIST%TNEIGHBOURLIST_FINAL+DFTBP_DFTB_PERIODIC 9   #  R      TNEIGHBOURLIST_FINAL+DFTBP_DFTB_PERIODIC >   à#  \   a   TNEIGHBOURLIST_FINAL%THIS+DFTBP_DFTB_PERIODIC -   <$  ³      MPIFX_COMM+MPIFX_COMM_MODULE 0   ï$  H   a   MPIFX_COMM%ID+MPIFX_COMM_MODULE 2   7%  H   a   MPIFX_COMM%SIZE+MPIFX_COMM_MODULE 2   %  H   a   MPIFX_COMM%RANK+MPIFX_COMM_MODULE 6   Ç%  H   a   MPIFX_COMM%LEADRANK+MPIFX_COMM_MODULE 2   &  H   a   MPIFX_COMM%LEAD+MPIFX_COMM_MODULE 2   W&  ]   a   MPIFX_COMM%INIT+MPIFX_COMM_MODULE 2   ´&  i      MPIFX_COMM_INIT+MPIFX_COMM_MODULE 7   '  X   a   MPIFX_COMM_INIT%SELF+MPIFX_COMM_MODULE 9   u'  @   a   MPIFX_COMM_INIT%COMMID+MPIFX_COMM_MODULE 8   µ'  @   a   MPIFX_COMM_INIT%ERROR+MPIFX_COMM_MODULE 3   õ'  ^   a   MPIFX_COMM%SPLIT+MPIFX_COMM_MODULE 3   S(        MPIFX_COMM_SPLIT+MPIFX_COMM_MODULE 8   Ø(  X   a   MPIFX_COMM_SPLIT%SELF+MPIFX_COMM_MODULE <   0)  @   a   MPIFX_COMM_SPLIT%SPLITKEY+MPIFX_COMM_MODULE ;   p)  @   a   MPIFX_COMM_SPLIT%RANKKEY+MPIFX_COMM_MODULE ;   °)  X   a   MPIFX_COMM_SPLIT%NEWCOMM+MPIFX_COMM_MODULE 9   *  @   a   MPIFX_COMM_SPLIT%ERROR+MPIFX_COMM_MODULE 8   H*  c   a   MPIFX_COMM%SPLIT_TYPE+MPIFX_COMM_MODULE 8   «*        MPIFX_COMM_SPLIT_TYPE+MPIFX_COMM_MODULE =   1+  X   a   MPIFX_COMM_SPLIT_TYPE%SELF+MPIFX_COMM_MODULE B   +  @   a   MPIFX_COMM_SPLIT_TYPE%SPLITTYPE+MPIFX_COMM_MODULE @   É+  @   a   MPIFX_COMM_SPLIT_TYPE%RANKKEY+MPIFX_COMM_MODULE @   	,  X   a   MPIFX_COMM_SPLIT_TYPE%NEWCOMM+MPIFX_COMM_MODULE >   a,  @   a   MPIFX_COMM_SPLIT_TYPE%ERROR+MPIFX_COMM_MODULE 2   ¡,  ]   a   MPIFX_COMM%FREE+MPIFX_COMM_MODULE 2   þ,  R      MPIFX_COMM_FREE+MPIFX_COMM_MODULE 7   P-  X   a   MPIFX_COMM_FREE%SELF+MPIFX_COMM_MODULE    ¨-  Õ       THALOGENX *   }.  ¬   !   THALOGENX%INTERACTIONTYPE     )/     !   THALOGENX%RADII !   ½/  §   !   THALOGENX%MAXDAB     d0  H   !   THALOGENX%NATOM !   ¬0  §   !   THALOGENX%CUTOFF %   S1  X   a   THALOGENX%GETRCUTOFF    «1  Z      GETRCUTOFF     2  W   a   GETRCUTOFF%THIS &   \2  Y   a   THALOGENX%GETENERGIES    µ2        GETENERGIES !   H3  W   a   GETENERGIES%THIS "   3     a   GETENERGIES%ATOME #   +4  ¤   a   GETENERGIES%COORDS $   Ï4     a   GETENERGIES%SPECIES "   [5  \   a   GETENERGIES%NEIGH )   ·5     a   GETENERGIES%IMG2CENTCELL '   C6  Z   a   THALOGENX%ADDGRADIENTS    6        ADDGRADIENTS "   17  W   a   ADDGRADIENTS%THIS $   7  ¤   a   ADDGRADIENTS%DERIVS $   ,8  ¤   a   ADDGRADIENTS%COORDS %   Ð8     a   ADDGRADIENTS%SPECIES #   \9  \   a   ADDGRADIENTS%NEIGH *   ¸9     a   ADDGRADIENTS%IMG2CENTCELL $   D:  W   a   THALOGENX%GETSTRESS    :        GETSTRESS    8;  W   a   GETSTRESS%THIS    ;  ¤   a   GETSTRESS%ST !   3<  ¤   a   GETSTRESS%COORDS     ×<  \   a   GETSTRESS%NEIGH "   3=     a   GETSTRESS%SPECIES '   ¿=     a   GETSTRESS%IMG2CENTCELL "   K>  @   a   GETSTRESS%CELLVOL    >  r       THALOGENX_INIT $   ý>  W   a   THALOGENX_INIT%THIS (   T?     a   THALOGENX_INIT%SPECIES0 ,   à?     a   THALOGENX_INIT%SPECIESNAMES !   p@        HALOGENXSPECIES1 !   B  å      HALOGENXSPECIES2    íC  À      MsObjComment 