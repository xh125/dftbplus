  �h  �   k820309    �          2021.9.0    �Ve                                                                                                          
       E:\workfiles\Githubfiles\dftbplus\build\src\dftbp\math\lapackroutines.f90 DFTBP_MATH_LAPACKROUTINES              MATINV SYMMATINV HERMATINV gen@GESV gen@GETRF gen@SYTRF gen@HETRF gen@SYTRI gen@HETRI gen@GETRI gen@LARNV gen@GESVD gen@POTRF gen@TRSM gen@GETRS                                                     
       DP RDP RSP                      @                              
       TSTATUS                      @                              
       ERROR WARNING                                                        u #GESV_REAL    #GESV_DBLE 
   #GESV_DCOMPLEX    #         @     @X                                                 #AA    #BB    #NEQUATION    #NSOLUTION    #IERROR 	             
D@                                                 	               &                   &                                                     
D@                                                 	               &                   &                                                     
 @                                                    
 @                                                    F @                               	            #         @     @X                             
                    #AA    #BB    #NEQUATION    #NSOLUTION    #IERROR              
D@                                                 
               &                   &                                                     
D@                                                 
               &                   &                                                     
 @                                                    
 @                                                    F @                                           #         @     @X                                                 #AA    #BB    #NEQUATION    #NSOLUTION    #IERROR              
D@                                                                &                   &                                                     
D@                                                                &                   &                                                     
 @                                                    
 @                                                    F @                                                                                                 u #GETRF_REAL    #GETRF_DBLE    #GETRF_COMPLEX "   #GETRF_DCOMPLEX (   #         @     @X                                                 #AA    #IPIV    #NROW    #NCOLUMN    #IERROR              
D@                                                 	 
              &                   &                                                     D @                                                                 &                                                     
 @                                                    
 @                                                    F @                                           #         @     @X                                                #AA    #IPIV    #NROW    #NCOLUMN     #IERROR !             
D@                                                 
               &                   &                                                     D @                                                                 &                                                     
 @                                                    
 @                                                     F @                               !            #         @     @X                             "                    #AA #   #IPIV $   #NROW %   #NCOLUMN &   #IERROR '             
D@                              #                                  &                   &                                                     D @                               $                                  &                                                     
 @                               %                     
 @                               &                     F @                               '            #         @     @X                             (                    #AA )   #IPIV *   #NROW +   #NCOLUMN ,   #IERROR -             
D@                              )                                  &                   &                                                     D @                               *                                  &                                                     
 @                               +                     
 @                               ,                     F @                               -                                                                  u #SYTRF_REAL .   #SYTRF_DREAL 4   #         @     @X                             .                    #AA /   #IPIV 0   #STATUS 1   #UPLO 3             
D@                              /                   	                &                   &                                                     D @                               0                    !              &                                                     D @                               1     p               #TSTATUS 2             
 @                               3                           #         @     @X                            4                    #AA 5   #IPIV 6   #STATUS 7   #UPLO 8             
D@                              5                   
 $              &                   &                                                     D @                               6                    %              &                                                     D @                               7     p               #TSTATUS 2             
 @                               8                                                                                 u #HETRF_CMPLX 9   #HETRF_DCMPLX >   #         @     @X                             9                    #AA :   #IPIV ;   #STATUS <   #UPLO =             
D@                              :                    (              &                   &                                                     D @                               ;                    )              &                                                     D @                               <     p               #TSTATUS 2             
 @                               =                           #         @     @X                            >                    #AA ?   #IPIV @   #STATUS A   #UPLO B             
D@                              ?                    ,              &                   &                                                     D @                               @                    -              &                                                     D @                               A     p               #TSTATUS 2             
 @                               B                                                                                 u #SYTRI_REAL C   #SYTRI_DREAL H   #         @     @X                             C                    #AA D   #IPIV E   #STATUS F   #UPLO G             
@@                              D                   	 0             &                   &                                                     
@ @                               E                    1             &                                                     D @                               F     p               #TSTATUS 2             
 @                               G                           #         @     @X                            H                    #AA I   #IPIV J   #STATUS K   #UPLO L             
@@                              I                   
 3             &                   &                                                     
@ @                               J                    4             &                                                     D @                               K     p               #TSTATUS 2             
 @                               L                                                                                 u #HETRI_CMPLX M   #HETRI_DCMPLX R   #         @     @X                             M                    #AA N   #IPIV O   #STATUS P   #UPLO Q             
@@                              N                    6             &                   &                                                     
@ @                               O                    7             &                                                     D @                               P     p               #TSTATUS 2             
 @                               Q                           #         @     @X                            R                    #AA S   #IPIV T   #STATUS U   #UPLO V             
@@                              S                    9             &                   &                                                     
@ @                               T                    :             &                                                     D @                               U     p               #TSTATUS 2             
 @                               V                                                                                 u #GETRI_REAL W   #GETRI_DBLE \   #         @     @X                             W                    #AA X   #IPIV Y   #NROW Z   #IERROR [             
D@                              X                   	               &                   &                                                     
@ @                               Y                                 &                                                     
 @                               Z                     F @                               [            #         @     @X                            \                    #AA ]   #IPIV ^   #NROW _   #IERROR `             
D@                              ]                   
               &                   &                                                     
@ @                               ^                                 &                                                     
 @                               _                     F @                               `                                                                   u #LARNV_REAL a   #LARNV_DBLE e   #LARNV_CPLX i   #LARNV_DBLECPLX m   #         @     @X                             a                    #IDIST b   #ISEED c   #X d             
@ @                               b                     
D @                               c                    <    p          p            p                                    D@                              d                   	 =              &                                           #         @     @X                             e                    #IDIST f   #ISEED g   #X h             
@ @                               f                     
D @                               g                    >    p          p            p                                    D@                              h                   
 ?              &                                           #         @     @X                             i                    #IDIST j   #ISEED k   #X l             
@ @                               j                     
D @                               k                    @    p          p            p                                    D@                              l                    A              &                                           #         @     @X                             m                    #IDIST n   #ISEED o   #X p             
@ @                               n                     
D @                               o                    B    p          p            p                                    D@                              p                    C              &                                                                                                  u #SGESVD_REAL q   #DGESVD_DBLE v   #CGESVD_CPLX {   #ZGESVD_DBLECPLX �   #         @     @X                             q                    #A r   #U s   #SIGMA t   #VT u             
D@                              r                   	 D              &                   &                                                     D@                              s                   	 E              &                   &                                                     D @                              t                   	 F              &                                                     D@                              u                   	 G              &                   &                                           #         @     @X                             v                    #A w   #U x   #SIGMA y   #VT z             
D@                              w                   
 I              &                   &                                                     D@                              x                   
 J              &                   &                                                     D @                              y                   
 K              &                                                     D@                              z                   
 L              &                   &                                           #         @     @X                             {                    #A |   #U }   #SIGMA ~   #VT              
D@                              |                    N              &                   &                                                     D@                              }                    O              &                   &                                                     D @                              ~                   	 P              &                                                     D@                                                  Q              &                   &                                           #         @     @X                             �                    #A �   #U �   #SIGMA �   #VT �             
D@                              �                    T              &                   &                                                     D@                              �                    U              &                   &                                                     D @                              �                   
 V              &                                                     D@                              �                    W              &                   &                                                                                                  u #SPOTRF_REAL �   #DPOTRF_DBLE �   #         @     @X                             �                    #B �   #UPLO �   #INFO �             
D@                              �                   	 Z              &                   &                                                     
 @                               �                                     F @                               �            #         @     @X                             �                    #B �   #UPLO �   #INFO �             
D@                              �                   
 [              &                   &                                                     
 @                               �                                     F @                               �                                                                   u #STRSM_REAL �   #DTRSM_DBLE �   #         @     @X                             �                 	   #SIDE �   #A �   #B �   #M �   #N �   #DIAG �   #ALPHA �   #TRANSA �   #UPLO �             
@ @                               �                                     
D@                              �                   	 \              &                   &                                                     
D@                              �                   	 ]              &                   &                                                     
@ @                               �                     
@ @                               �                     
@ @                               �                                     
@ @                              �     	                
 @                               �                                     
B @                               �                           #         @     @X                             �                 	   #SIDE �   #A �   #B �   #M �   #N �   #DIAG �   #ALPHA �   #TRANSA �   #UPLO �             
@ @                               �                                     
D@                              �                   
 ^              &                   &                                                     
D@                              �                   
 _              &                   &                                                     
@ @                               �                     
@ @                               �                     
@ @                               �                                     
@ @                              �     
                
 @                               �                                     
B @                               �                                                                                 u #GETRS_DBLE �   #GETRS1_DBLE �   #GETRS_REAL �   #GETRS1_REAL �   #         @     @X                            �                    #AMAT �   #IPIV �   #BMAT �   #TRANS �   #IERROR �             
@@                              �                   
 `             &                   &                                                     
@ @                               �                    a             &                                                     
D@                              �                   
 b              &                   &                                                     
 @                              �                                     F @                               �            #         @     @X                             �                    #AMAT �   #IPIV �   #BVEC �   #TRANS �   #IERROR �             
  @                              �                   
 c             &                   &                                                     
  @                               �                    d             &                                                     
`                              �                   
 e              &                                                     
 @                              �                                     F @                               �            #         @     @X                            �                    #AMAT �   #IPIV �   #BMAT �   #TRANS �   #IERROR �             
@@                              �                   	 g             &                   &                                                     
@ @                               �                    h             &                                                     
D@                              �                   	 i              &                   &                                                     
 @                              �                                     F @                               �            #         @     @X                             �                    #AMAT �   #IPIV �   #BVEC �   #TRANS �   #IERROR �             
  @                              �                   	 j             &                   &                                                     
  @                               �                    k             &                                                     
`                              �                   	 l              &                                                     
 @                              �                                     F @                               �                          @�                                       u #WARNING_SINGLE �   #WARNING_ARRAY �   #         @     @                            �                    #MESSAGE �             
                                �                    1 #         @     @                            �                    #MESSAGES �   ,          
                               �                                  &                                           1               @�                                      u #ERROR_SINGLE �   #ERROR_ARRAY �   #         @     @                           �                    #MESSAGE �             
                                �                    1 #         @     @                            �                    #MESSAGES �   ,          
                               �                                  &                                           1               @  @              A           2     'p                    #CODE �   #MESSAGE �   #FILE �   #LINE �   #SETERROR �   #HASERROR �   #ISOK �               � $                              �                                                                                                 0                � $                      @      �                                       � $                      @      �             8                          � $                              �     h                                                                                           0    1         �   � $                     �      �                  #SETERROR �   #         @     @                           �                    #THIS �   #CODE �   #MESSAGE �   #FILE �   #LINE �             
                                �     p               #TSTATUS 2             
                                  �                     
                                �                    1           
                                �                    1           
                                  �           1         �   � $                    �      �                  #HASERROR �   %         @   @                           �                           #THIS �             
                                 �     p              #TSTATUS 2   1         �   � $                     �      �                  #ISOK �   %         @   @                            �                           #THIS �             
                                 �     p              #TSTATUS 2   #         @                                   �                    #AA �   #NROW �   #IERROR �             
D@                              �                   
               &                   &                                                     
 @                               �                     F @                               �            #         @                                   �                    #AA �   #STATUS �   #UPLO �             
D@                              �                   
               &                   &                                                     D @                               �     p               #TSTATUS 2             
 @                               �                           #         @                                   �                    #AA �   #STATUS �   #UPLO �             
D@                              �                                  &                   &                                                     D @                               �     p               #TSTATUS 2             
 @                               �                           *         � n                         M              Cifmodintr.lib  � n                           M              Cifmodintr.lib                                           �   l      fn#fn /     �   b   uapp(DFTBP_MATH_LAPACKROUTINES &   �  K   J  DFTBP_COMMON_ACCURACY $   �  H   J  DFTBP_COMMON_STATUS !   @  N   J  DFTBP_IO_MESSAGE    �  q       gen@GESV    �  �      GESV_REAL    �  �   a   GESV_REAL%AA    %  �   a   GESV_REAL%BB $   �  @   a   GESV_REAL%NEQUATION $   	  @   a   GESV_REAL%NSOLUTION !   I  @   a   GESV_REAL%IERROR    �  �      GESV_DBLE      �   a   GESV_DBLE%AA    �  �   a   GESV_DBLE%BB $   S  @   a   GESV_DBLE%NEQUATION $   �  @   a   GESV_DBLE%NSOLUTION !   �  @   a   GESV_DBLE%IERROR      �      GESV_DCOMPLEX !   �  �   a   GESV_DCOMPLEX%AA !   9	  �   a   GESV_DCOMPLEX%BB (   �	  @   a   GESV_DCOMPLEX%NEQUATION (   
  @   a   GESV_DCOMPLEX%NSOLUTION %   ]
  @   a   GESV_DCOMPLEX%IERROR    �
  �       gen@GETRF    $  }      GETRF_REAL    �  �   a   GETRF_REAL%AA     E  �   a   GETRF_REAL%IPIV     �  @   a   GETRF_REAL%NROW #     @   a   GETRF_REAL%NCOLUMN "   Q  @   a   GETRF_REAL%IERROR    �  }      GETRF_DBLE      �   a   GETRF_DBLE%AA     �  �   a   GETRF_DBLE%IPIV     >  @   a   GETRF_DBLE%NROW #   ~  @   a   GETRF_DBLE%NCOLUMN "   �  @   a   GETRF_DBLE%IERROR    �  }      GETRF_COMPLEX !   {  �   a   GETRF_COMPLEX%AA #     �   a   GETRF_COMPLEX%IPIV #   �  @   a   GETRF_COMPLEX%NROW &   �  @   a   GETRF_COMPLEX%NCOLUMN %   +  @   a   GETRF_COMPLEX%IERROR    k  }      GETRF_DCOMPLEX "   �  �   a   GETRF_DCOMPLEX%AA $   �  �   a   GETRF_DCOMPLEX%IPIV $     @   a   GETRF_DCOMPLEX%NROW '   X  @   a   GETRF_DCOMPLEX%NCOLUMN &   �  @   a   GETRF_DCOMPLEX%IERROR    �  a       gen@SYTRF    9  p      SYTRF_REAL    �  �   a   SYTRF_REAL%AA     M  �   a   SYTRF_REAL%IPIV "   �  U   a   SYTRF_REAL%STATUS     .  P   a   SYTRF_REAL%UPLO    ~  p      SYTRF_DREAL    �  �   a   SYTRF_DREAL%AA !   �  �   a   SYTRF_DREAL%IPIV #     U   a   SYTRF_DREAL%STATUS !   s  P   a   SYTRF_DREAL%UPLO    �  c       gen@HETRF    &  p      HETRF_CMPLX    �  �   a   HETRF_CMPLX%AA !   :  �   a   HETRF_CMPLX%IPIV #   �  U   a   HETRF_CMPLX%STATUS !     P   a   HETRF_CMPLX%UPLO    k  p      HETRF_DCMPLX     �  �   a   HETRF_DCMPLX%AA "     �   a   HETRF_DCMPLX%IPIV $     U   a   HETRF_DCMPLX%STATUS "   `  P   a   HETRF_DCMPLX%UPLO    �  a       gen@SYTRI      p      SYTRI_REAL    �  �   a   SYTRI_REAL%AA     %   �   a   SYTRI_REAL%IPIV "   �   U   a   SYTRI_REAL%STATUS     !  P   a   SYTRI_REAL%UPLO    V!  p      SYTRI_DREAL    �!  �   a   SYTRI_DREAL%AA !   j"  �   a   SYTRI_DREAL%IPIV #   �"  U   a   SYTRI_DREAL%STATUS !   K#  P   a   SYTRI_DREAL%UPLO    �#  c       gen@HETRI    �#  p      HETRI_CMPLX    n$  �   a   HETRI_CMPLX%AA !   %  �   a   HETRI_CMPLX%IPIV #   �%  U   a   HETRI_CMPLX%STATUS !   �%  P   a   HETRI_CMPLX%UPLO    C&  p      HETRI_DCMPLX     �&  �   a   HETRI_DCMPLX%AA "   W'  �   a   HETRI_DCMPLX%IPIV $   �'  U   a   HETRI_DCMPLX%STATUS "   8(  P   a   HETRI_DCMPLX%UPLO    �(  `       gen@GETRI    �(  p      GETRI_REAL    X)  �   a   GETRI_REAL%AA     �)  �   a   GETRI_REAL%IPIV     �*  @   a   GETRI_REAL%NROW "   �*  @   a   GETRI_REAL%IERROR    +  p      GETRI_DBLE    x+  �   a   GETRI_DBLE%AA     ,  �   a   GETRI_DBLE%IPIV     �,  @   a   GETRI_DBLE%NROW "   �,  @   a   GETRI_DBLE%IERROR    (-  �       gen@LARNV    �-  e      LARNV_REAL !   .  @   a   LARNV_REAL%IDIST !   Q.  �   a   LARNV_REAL%ISEED    �.  �   a   LARNV_REAL%X    q/  e      LARNV_DBLE !   �/  @   a   LARNV_DBLE%IDIST !   0  �   a   LARNV_DBLE%ISEED    �0  �   a   LARNV_DBLE%X    61  e      LARNV_CPLX !   �1  @   a   LARNV_CPLX%IDIST !   �1  �   a   LARNV_CPLX%ISEED    o2  �   a   LARNV_CPLX%X    �2  e      LARNV_DBLECPLX %   `3  @   a   LARNV_DBLECPLX%IDIST %   �3  �   a   LARNV_DBLECPLX%ISEED !   44  �   a   LARNV_DBLECPLX%X    �4  �       gen@GESVD    H5  i      SGESVD_REAL    �5  �   a   SGESVD_REAL%A    U6  �   a   SGESVD_REAL%U "   �6  �   a   SGESVD_REAL%SIGMA    �7  �   a   SGESVD_REAL%VT    )8  i      DGESVD_DBLE    �8  �   a   DGESVD_DBLE%A    69  �   a   DGESVD_DBLE%U "   �9  �   a   DGESVD_DBLE%SIGMA    f:  �   a   DGESVD_DBLE%VT    
;  i      CGESVD_CPLX    s;  �   a   CGESVD_CPLX%A    <  �   a   CGESVD_CPLX%U "   �<  �   a   CGESVD_CPLX%SIGMA    G=  �   a   CGESVD_CPLX%VT     �=  i      ZGESVD_DBLECPLX "   T>  �   a   ZGESVD_DBLECPLX%A "   �>  �   a   ZGESVD_DBLECPLX%U &   �?  �   a   ZGESVD_DBLECPLX%SIGMA #   (@  �   a   ZGESVD_DBLECPLX%VT    �@  b       gen@POTRF    .A  c      SPOTRF_REAL    �A  �   a   SPOTRF_REAL%B !   5B  P   a   SPOTRF_REAL%UPLO !   �B  @   a   SPOTRF_REAL%INFO    �B  c      DPOTRF_DBLE    (C  �   a   DPOTRF_DBLE%B !   �C  P   a   DPOTRF_DBLE%UPLO !   D  @   a   DPOTRF_DBLE%INFO    \D  `       gen@TRSM    �D  �      STRSM_REAL     UE  P   a   STRSM_REAL%SIDE    �E  �   a   STRSM_REAL%A    IF  �   a   STRSM_REAL%B    �F  @   a   STRSM_REAL%M    -G  @   a   STRSM_REAL%N     mG  P   a   STRSM_REAL%DIAG !   �G  @   a   STRSM_REAL%ALPHA "   �G  P   a   STRSM_REAL%TRANSA     MH  P   a   STRSM_REAL%UPLO    �H  �      DTRSM_DBLE     6I  P   a   DTRSM_DBLE%SIDE    �I  �   a   DTRSM_DBLE%A    *J  �   a   DTRSM_DBLE%B    �J  @   a   DTRSM_DBLE%M    K  @   a   DTRSM_DBLE%N     NK  P   a   DTRSM_DBLE%DIAG !   �K  @   a   DTRSM_DBLE%ALPHA "   �K  P   a   DTRSM_DBLE%TRANSA     .L  P   a   DTRSM_DBLE%UPLO    ~L  �       gen@GETRS     M  }      GETRS_DBLE     }M  �   a   GETRS_DBLE%AMAT     !N  �   a   GETRS_DBLE%IPIV     �N  �   a   GETRS_DBLE%BMAT !   QO  P   a   GETRS_DBLE%TRANS "   �O  @   a   GETRS_DBLE%IERROR    �O  }      GETRS1_DBLE !   ^P  �   a   GETRS1_DBLE%AMAT !   Q  �   a   GETRS1_DBLE%IPIV !   �Q  �   a   GETRS1_DBLE%BVEC "   R  P   a   GETRS1_DBLE%TRANS #   jR  @   a   GETRS1_DBLE%IERROR    �R  }      GETRS_REAL     'S  �   a   GETRS_REAL%AMAT     �S  �   a   GETRS_REAL%IPIV     WT  �   a   GETRS_REAL%BMAT !   �T  P   a   GETRS_REAL%TRANS "   KU  @   a   GETRS_REAL%IERROR    �U  }      GETRS1_REAL !   V  �   a   GETRS1_REAL%AMAT !   �V  �   a   GETRS1_REAL%IPIV !   8W  �   a   GETRS1_REAL%BVEC "   �W  P   a   GETRS1_REAL%TRANS #   X  @   a   GETRS1_REAL%IERROR -   TX  g       gen@WARNING+DFTBP_IO_MESSAGE 0   �X  U      WARNING_SINGLE+DFTBP_IO_MESSAGE 8   Y  L   a   WARNING_SINGLE%MESSAGE+DFTBP_IO_MESSAGE /   \Y  V      WARNING_ARRAY+DFTBP_IO_MESSAGE 8   �Y  �   a   WARNING_ARRAY%MESSAGES+DFTBP_IO_MESSAGE +   BZ  c       gen@ERROR+DFTBP_IO_MESSAGE .   �Z  U      ERROR_SINGLE+DFTBP_IO_MESSAGE 6   �Z  L   a   ERROR_SINGLE%MESSAGE+DFTBP_IO_MESSAGE -   F[  V      ERROR_ARRAY+DFTBP_IO_MESSAGE 6   �[  �   a   ERROR_ARRAY%MESSAGES+DFTBP_IO_MESSAGE ,   ,\  �      TSTATUS+DFTBP_COMMON_STATUS 1   �\  �   a   TSTATUS%CODE+DFTBP_COMMON_STATUS 4   r]  P   a   TSTATUS%MESSAGE+DFTBP_COMMON_STATUS 1   �]  P   a   TSTATUS%FILE+DFTBP_COMMON_STATUS 1   ^  �   a   TSTATUS%LINE+DFTBP_COMMON_STATUS 5   �^  V   a   TSTATUS%SETERROR+DFTBP_COMMON_STATUS -   _  }      SETERROR+DFTBP_COMMON_STATUS 2   �_  U   a   SETERROR%THIS+DFTBP_COMMON_STATUS 2   �_  @   a   SETERROR%CODE+DFTBP_COMMON_STATUS 5   `  L   a   SETERROR%MESSAGE+DFTBP_COMMON_STATUS 2   k`  L   a   SETERROR%FILE+DFTBP_COMMON_STATUS 2   �`  @   a   SETERROR%LINE+DFTBP_COMMON_STATUS 5   �`  V   a   TSTATUS%HASERROR+DFTBP_COMMON_STATUS -   Ma  Z      HASERROR+DFTBP_COMMON_STATUS 2   �a  U   a   HASERROR%THIS+DFTBP_COMMON_STATUS 1   �a  R   a   TSTATUS%ISOK+DFTBP_COMMON_STATUS )   Nb  Z      ISOK+DFTBP_COMMON_STATUS .   �b  U   a   ISOK%THIS+DFTBP_COMMON_STATUS    �b  f       MATINV    cc  �   a   MATINV%AA    d  @   a   MATINV%NROW    Gd  @   a   MATINV%IERROR    �d  f       SYMMATINV    �d  �   a   SYMMATINV%AA !   �e  U   a   SYMMATINV%STATUS    �e  P   a   SYMMATINV%UPLO    6f  f       HERMATINV    �f  �   a   HERMATINV%AA !   @g  U   a   HERMATINV%STATUS    �g  P   a   HERMATINV%UPLO    �g  �      MsObjComment 