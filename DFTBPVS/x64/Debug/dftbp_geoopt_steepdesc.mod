  u  #   k820309    Ù          2021.9.0    ýØVe                                                                                                          
       E:\workfiles\Githubfiles\dftbplus\build\src\dftbp\geoopt\steepdesc.f90 DFTBP_GEOOPT_STEEPDESC              TSTEEPDESCINPUT TSTEEPDESC TSTEEPDESC_INIT                                                     
       DP                      @                              
       TOPTIMIZER TOPTIMIZERINPUT               @  @                              '                      #STEP    #RESET 
   1         À    $                                             #STEP    #         @     @                                	               #THIS    #VAL    #GRAD    #DISPL 	             
                                                    #TOPTIMIZER              
                                     
                
                                                   
              &                                                                                    	                   
               &                                           1         À    $                           
                  #RESET    #         @     @                                	               #THIS              
                                                    #TOPTIMIZER                  @  @                              '                                          @                               '                    #TOPTIMIZERINPUT    #SCALINGFACTOR                  $                                                          #TOPTIMIZERINPUT                  $                                             
                     @                               '                    #TOPTIMIZER    #NVAR    #SCALINGFACTOR    #STEP    #RESET                  $                                                          #TOPTIMIZER                  $                                                               $                                            
   1         À    $                                             #STEP    #         @     @                                                 #THIS    #VAL    #GRAD    #DISPL              
                                                    #TSTEEPDESC              
                                      
                
                                                    
              &                                                     D                                                   
               &                                           1         À    $                                             #RESET    #         @     @                                                 #THIS              
                                                    #TSTEEPDESC    #         @                                                       #THIS    #INPUT     #NVAR !             D                                                     #TSTEEPDESC              
                                                      #TSTEEPDESCINPUT              
                                  !                  f      fn#fn ,     ;   b   uapp(DFTBP_GEOOPT_STEEPDESC &   A  C   J  DFTBP_COMMON_ACCURACY '     [   J  DFTBP_GEOOPT_OPTIMIZER 2   ß  e      TOPTIMIZER+DFTBP_GEOOPT_OPTIMIZER 7   D  R   a   TOPTIMIZER%STEP+DFTBP_GEOOPT_OPTIMIZER ,     p      STEP+DFTBP_GEOOPT_OPTIMIZER 1     X   a   STEP%THIS+DFTBP_GEOOPT_OPTIMIZER 0   ^  @   a   STEP%VAL+DFTBP_GEOOPT_OPTIMIZER 1        a   STEP%GRAD+DFTBP_GEOOPT_OPTIMIZER 2   *     a   STEP%DISPL+DFTBP_GEOOPT_OPTIMIZER 8   ¶  S   a   TOPTIMIZER%RESET+DFTBP_GEOOPT_OPTIMIZER -   	  R      RESET+DFTBP_GEOOPT_OPTIMIZER 2   [  X   a   RESET%THIS+DFTBP_GEOOPT_OPTIMIZER 7   ³  P      TOPTIMIZERINPUT+DFTBP_GEOOPT_OPTIMIZER       x       TSTEEPDESCINPUT 0   {  e   a   TSTEEPDESCINPUT%TOPTIMIZERINPUT .   à  H   a   TSTEEPDESCINPUT%SCALINGFACTOR    (         TSTEEPDESC &   º  `   a   TSTEEPDESC%TOPTIMIZER       H   a   TSTEEPDESC%NVAR )   b  H   a   TSTEEPDESC%SCALINGFACTOR     ª  R   a   TSTEEPDESC%STEP    ü  p      STEP    l	  X   a   STEP%THIS    Ä	  @   a   STEP%VAL    
     a   STEP%GRAD    
     a   STEP%DISPL !     S   a   TSTEEPDESC%RESET    o  R      RESET    Á  X   a   RESET%THIS       g       TSTEEPDESC_INIT %     X   a   TSTEEPDESC_INIT%THIS &   Ø  ]   a   TSTEEPDESC_INIT%INPUT %   5  @   a   TSTEEPDESC_INIT%NVAR 