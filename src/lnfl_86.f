C     path:      /home/rc1/aer_lnfl/src/SCCS/s.lnfl.f
C     revision:  2.7
C     created:   03/18/96  18:06:58
C     presently: 03/18/96  18:08:15
      PROGRAM LNFL                                                       LN00010
C                                                                        LN00020
C**********************************************************************  LN00030
C                                                                        LN00040
C                            LNFL                    21 OCTOBER 1991     LN00050
C                                                                        LN00060
C    THIS PROGRAM CREATES A LINE DATA FILE FOR LBLRTM.                   LN00070
C                                                                        LN00080
C                                                                        LN00090
C    FOR USE WITH 1986 TAPE FORMAT FOR TAPE1 AND SUPPLEMENTAL OR         LN00100
C    REPLACEMENT LINE DATA ON TAPE2.                                     LN00110
C                                                                        LN00120
C                                                                        LN00130
C    DEFAULT IMPLEMENTATION OF THIS PROGRAM CAUSES LINE FILE DATA        LN00140
C    INCLUDING LINE COUPLING INFORMATION, INTERNALLY STORED IN THIS      LN00150
C    PROGRAM, TO BE WRITTEN TO TAPE2 AND INCORPORATED INTO THE LBLRTM    LN00160
C    LINE FILE, TAPE3. THE INTERNALLY STORED LINE DATA REPLACES LINE     LN00170
C    DATA FROM TAPE1 FOR IDENTICAL TRANSITIONS.                          LN00180
C                                                                        LN00190
C    INTERNAL LINE COUPLING DATA IS INCLUDED FOR OXYGEN IN THE           LN00200
C     0., 2. (60GHZ) AND  4. (120GHZ) CM-1 SPECTRAL REGION AND FOR THE   LN00210
C    CARBON DIOXIDE Q-BRANCHES AT 618., 667., 720., 721.  AND 791. CM-1  LN00220
C    THE DATA IS PROVIDED FOR THE MAIN ISOTOPES ONLY. THE LINE COUPLING  LN00230
C    COEFFICIENTS ARE DUE TO HOKE AND CLOUGH, 1988.                      LN00240
C                                                                        LN00250
C    LNFL ALSO TRANSFERS TO TAPE3 THE ISOTOPE NUMBER FOR EACH LINE.      LN00260
C                                                                        LN00270
C    THE FILES TAPE1 AND TAPE2 MUST INCLUDE THE LINE PARAMETERS          LN00280
C    REQUIRED FOR THE MOLECULAR TYPES AND WAVENUMBER RANGE SELECTED.     LN00290
C                                                                        LN00300
C______________________________________________________________________  LN00310
C                                                                        LN00320
C                             TAPE1                                      LN00330
C                                                                        LN00340
C    THIS FILE CONTAINS THE AFGL OR EQUIVALENT LINE DATA IN CODED        LN00350
C        FORMAT FOR THE MAIN AND TRACE ATMOSPHERIC MOLECULES.            LN00360
C                                                                        LN00370
C    THE REFERENCE FOR THE 1986 AFGL LINE DATA IS                        LN00380
C                                                                        LN00390
C       L.S. ROTHMAN, R. R. GAMACHE, A. GOLDMAN, L. R. BROWN,            LN00400
C       R. A. TOTH, H. M. PICKETT, R. L. POYNTER, J.-M. FLAUD,           LN00410
C       C. CAMY-PEYRET, A. BARBE, N. HUSSON, C. P. RINSLAND,             LN00420
C       AND M. A. H. SMITH                                               LN00430
C                                                                        LN00440
C                  'THE HITRAN DATABASE: 1986 EDITION,'                  LN00450
C                     APPLIED OPTICS 26,4058 (1987).                     LN00460
C                                                                        LN00470
C                                                                        LN00480
C    TAPE1 IS ASSUMED TO BE BLOCK FORMATTED WITH 51 TRANSITIONS          LN00490
C    PER RECORD. IF THE NBLK1 OPTION IS SELECTED ON INPUT RECORD 2,      LN00500
C    ONE (1) TRANSITION PER RECORD IS READ (NO BLOCKING ON TAPE 1).      LN00510
C                                                                        LN00520
C    THERE ARE 100 CHARACTERS FOR EACH TRANSITION.                       LN00530
C    THE DATA FOR EACH TRANSITION INCLUDE MOLECULE IDENTIFICATION,       LN00540
C    LINE FREQUENCY, INTENSITY, AIR HALF-WIDTH, SELF HALFWIDTH,          LN00550
C    LOWER STATE ENERGY, TEMPERATUE DEPENDENCE OF THE AIR HALFWIDTH,     LN00560
C    PRESSURE SHIFT, UPPER AND LOWER STATE VIBRATIONAL AND ROTATIONAL    LN00570
C    IDENTIFICATIONS, REFERENCE PARAMETERS AND A FLAG FOR LINE           LN00580
C    COUPLING.                                                           LN00590
C                                                                        LN00600
C    MOLECULE NUMBERS 1 THROUGH 32 MAY BE SELECTED.                      LN00610
C                                                                        LN00620
C                                                                        LN00630
C    THE VARIABLES AND THE FORMAT FOR THE TRANSITIONS ON TAPE1 ARE       LN00640
C                                                                        LN00650
C ISO,VNU,STR,TRANS,HWHM,HWHMS,ENERGY,TDEP,SHIFT,IVUP,IVLO,CUP,CLO,IFLG  LN00660
C                                                                        LN00670
C 2X,I1,F12.6,2E10.3, 2F5.4,    F10.4,F4.2, F8.6,   2I3,     2A9,7X,I2   LN00680
C                                                                        LN00690
C                                                                        LN00700
C______________________________________________________________________  LN00710
C                                                                        LN00720
C                                                                        LN00730
C              TAPE2                                                     LN00740
C                                                                        LN00750
C    TAPE2 IS AVAILABLE FOR THE USER TO PROVIDE ALTERNATE LINE DATA      LN00760
C               (REPLACEMENT OR SUPPLEMENTAL)                            LN00770
C                                                                        LN00780
C    MOLECULE NUMBERS 1 THROUGH 32 MAY BE SELECTED.                      LN00790
C                                                                        LN00800
C    TWO FORMAT OPTIONS ARE AVAILABLE: OPTION SELECTED ON RECORD 2.      LN00810
C                                                                        LN00820
C                                                                        LN00830
C    F80        1982 AFGL FORMAT                                         LN00840
C                                                                        LN00850
C    F100       1986 AFGL FORMAT (USE WITH HITRAN 1991)                  LN00860
C                                                                        LN00870
C               LINE COUPLING COEFFICIENTS MAY BE PROVIDED ON THIS FILE  LN00880
C                                                                        LN00890
C                                                                        LN00900
C    IF FORMAT F100 IS SELECTED, A BLOCKING OPTION IS AVAILABLE:         LN00910
C                                                                        LN00920
C        NBLK2       ONE (1) TRANSITION PER RECORD (NO BLOCKING)         LN00930
C                                                                        LN00940
C                    DEFAULT IS 51 TRANSITIONS PER RECORD                LN00950
C                                                                        LN00960
C______________________________________________________________________  LN00970
C                                                                        LN00980
C                                                                        LN00990
C              TAPE3                                                     LN01000
C                                                                        LN01010
C    LINE DATA FILE FOR LBLRTM; UNFORMATTED                              LN01020
C    TAPE3 NOT COMPATIBLE WITH FASCOD2 CODE                              LN01030
C                                                                        LN01040
C______________________________________________________________________  LN01050
C                                                                        LN01060
C                                                                        LN01070
C              TAPE7                                                     LN01080
C                                                                        LN01090
C    FORMATTED REPRESENTATION OF TAPE3 SELECTED BY OPTION 'LNOUT' ON     LN01100
C    RECORD 2.                                                           LN01110
C                                                                        LN01120
C    ONE TRANSITION PER RECORD IN SAME REPRESENTATION AS DATA ON         LN01130
C    ON TAPE1 AND TAPE2.                                                 LN01140
C                                                                        LN01150
C______________________________________________________________________  LN01160
C                                                                        LN01170
C                                                                        LN01180
C              TAPE10                                                    LN01190
C                                                                        LN01200
C    THIS FILE IS AN INTERMEDIATE UNFORMATTED FILE                       LN01210
C                                                                        LN01220
C**********************************************************************  LN01230
C                                                                        LN01240
C NOTE : IN LINE COUPLING MODE (DEFAULT), THE CORRESPONDING LINES ON     LN01250
C        THE TAPE1 LINE DATA FILE ARE REPLACED BY LINE DATA              LN01260
C        INTERNALLY STORED IN BLOCK DATA IN THIS PROGRAM CONTAINING      LN01270
C        THE Y'S AND THE G'S.                                            LN01280
C                                                                        LN01290
C**********************************************************************  LN01300
C-                                                                       LN01310
C-                      STATEMENT FLAGS                                  LN01320
C-                                                                       LN01330
C-    LNFL   HAS BEEN STRUCTURED TO HAVE ENHANCED PORTABILITY UNDER      LN01340
C-    FORTRAN 77.  FOUR FLAGS (COLUMN73) HAVE BEEN USED TO FACILITATE    LN01350
C-    PROGRAM CONVERSION.                                                LN01360
C-                                                                       LN01370
C-   &    IDENTIFIES STATEMENTS REQUIRED FOR WORD SIZE LESS THAN 8 CHAR  LN01380
C-               ALL STATEMENTS FLAGGED WITH & IN COLUMN 73 HAVE         LN01390
C-            C& STARTING IN COLUMN 1. THESE TWO CHARACTERS MUST         LN01400
C-               BE CHANGED TO BLANKS FOR COMPUTERS WITH WORD SIZE       LN01410
C-               LESS THAN 8 CHARACTERS.                                 LN01420
C-                                                                       LN01430
C-   !    IDENTIFIES STATEMENTS REQUIRED TO DOUBLE PRECISION THE         LN01440
C-               VARIABLES NEEDED FOR CALCULATIONS WHICH NEED MORE       LN01450
C-               THAN 32 BITS TO OBTAIN SUFFICIENT ACCURACY (I.E.        LN01460
C-               THE FREQUENCIES). STATEMENTS FLAGGED WITH ! HAVE        LN01470
C-            C! STARTING IN COLUMN 1. THESE TWO CHARACTERS SHOULD BE    LN01480
C-               CHANGED TO BLANKS FOR COMPUTERS HAVING SINGLE           LN01490
C-               PRECISION LESS THAN 10 SIGNIFICANT DIGITS.              LN01500
C-                                                                       LN01510
C-   #    IDENTIFIES STATEMENTS THAT MAY BE USEFUL FOR ACCELERATED       LN01520
C-               FILE DATA TRANSFER UNDER CDC AND OTHER OPERATING        LN01530
C-               SYSTEMS ALLOWING BUFFERED I/0.                          LN01540
C-                                                                       LN01550
C-   >    IDENTIFIES STATEMENTS THAT MAY BE USEFUL FOR CONVERSION,       LN01560
C-               TYPICALLY SYSTEM SPECIFIC CALLS (I.E. DATE, TIME,       LN01570
C-               CPU TIME, RANDOM NUMBER, ETC.).                         LN01580
C-                                                                       LN01590
C----------------------------------------------------------------------  LN01600
C                                                                        LN01610
      IMPLICIT DOUBLE PRECISION (V)                                     !LN01620
C                                                                        LN01630
      DOUBLE PRECISION HID,HTIME,HDATE,HID1,HMOL                        &LN01640
C                                                                        LN01650
      CHARACTER*8 GREJ,GNLTE,GREJNL                                      LN01660
C                                                                        LN01670
      CHARACTER HF80*3,HF100*4,HNOCPL*5,HREJ*3,HNLTE*4,HOLIND*40         LN01680
      CHARACTER*5 HNBLK1,HNBLK2,HLNOUT,HH86T1,HH86T2                     LN01690
      CHARACTER*27 QUANT1,QUANTC                                         LN01700
      CHARACTER*100 ALIN1,ALIN2,ALINC,ALIN                               LN01710
      character*1 cdum
C                                                                        LN01720
      COMMON /LCHAR/ ALIN1(51),ALIN2(40),ALINC(51),ALIN(250)             LN01730
C                                                                        LN01740
      COMMON VNU3(250),STR3(250),ALF3(250),EPP3(250),MOL3(250),          LN01750
     *       HWHMS(250),TMPALF(250),PSHIFT(250),IFLG(250),LSTW2          LN01760
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN01770
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN01780
      COMMON /HBLOCK/ INBLK1,INBLK2,IO2BND,I86T1,I86T2                   LN01790
C                                                                        LN01800
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN01810
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN01820
     *               IREC,IRECTL,HID1(2),LSTWD                           LN01830
C                                                                        LN01840
      EQUIVALENCE (HID1(1),HDATE) , (HID1(2),HTIME)                      LN01850
C                                                                        LN01860
      COMMON /BUFIDC/ CMOL(64),CHID10,CHID08                             LN01870
      CHARACTER CMOL*6,CHID10*8,CHID08*8,CFORM*11                        LN01880
      COMMON /UNITS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2           LN01890
      COMMON /MANE/ VNU1(51),STR1(51),ALF1(51),EPP1(51),MOL1(51),        LN01900
     *              HWHM1(51),TMPAL1(51),PSHIF1(51),IFG1(51),            LN01910
     *              MIND1(35),IOUT(51)                                   LN01920
      COMMON /MAINC/ VNUC(51),STRC(51),ALFC(51),EPPC(51),MOLC(51),       LN01930
     *               HWHMC(51),TMPALC(51),PSHIFC(51),IFGC(51),           LN01940
     *               MINDC(35),IOUTC(51)                                 LN01950
      COMMON /TRAC/ VNU2(40),STR2(40),ALF2(40),EPP2(40),MOL2(40),        LN01960
     *              HWHM2(40),TMPAL2(40),PSHIF2(40),IFG2(40),            LN01970
     *              MIND2(35)                                            LN01980
      COMMON /CPLMOL/ MOLCPL(35),NCPL                                    LN01990
      COMMON /SREJ/ SR(64),SRD(64),TALF(64)                              LN02000
      COMMON /ICN/ ILIN3,NMAX,NBLOCK                                     LN02010
      COMMON /QUANT/ QUANT1(51),QUANTC(51)                               LN02020
C                                                                        LN02030
      DIMENSION MOLCNT(64),IID(10),RCDHDR(5)                             LN02040
      DIMENSION IWD1(2),IWD2(2500),AMOL1(51),AMOL2(40),AMOLC(51)         LN02050
C                                                                        LN02060
      EQUIVALENCE (IWD1(1),VLO) , (IWD2(1),VNU3(1))                      LN02070
      EQUIVALENCE (IID(1),HID(1)) , (RCDHDR(1),VLO)                      LN02080
      EQUIVALENCE (MOL1(1),AMOL1(1)) , (MOL2(1),AMOL2(1))                LN02090
      EQUIVALENCE (MOLC(1),AMOLC(1))                                     LN02100
C                                                                        LN02110
      DATA HF80 / 'F80'/,HF100 / 'F100'/,HNOCPL / 'NOCPL'/,              LN02120
     *     HLNOUT / 'LNOUT'/                                             LN02130
      DATA HREJ / 'REJ'/,HNLTE / 'NLTE'/,HNBLK1 / 'NBLK1'/,              LN02140
     *     HNBLK2 / 'NBLK2'/                                             LN02150
      DATA HH86T1 / 'H86T1'/,HH86T2 / 'H86T2'/
      DATA GREJ / ' REJ    '/,GNLTE / ' NLTE   '/,GREJNL / 'NLTE REJ'/   LN02160
      DATA MOLCNT / 64*0 /                                               LN02170
      DATA VLST1 / -1. /,VLST2 / -2. /                                   LN02180
C                                                                        LN02190
C#    DATA CFORM/'BUFFERED   '/                                          LN02200
      DATA CFORM / 'UNFORMATTED'/                                        LN02210
C                                                                        LN02220
      CALL CPUTIM (TIME0)                                                LN02230
C                                                                        LN02240
      NWDLIN = NWDL(IID,LSTWD)                                           LN02250
      NBLOCK = 0                                                         LN02260
      LRC = NWDL(IWD1,LSTW1)                                             LN02270
      ILNGTH = NWDL(IWD2,LSTW2)                                          LN02280
      RADCN2 = PLANCK*CLIGHT/BOLTZ                                       LN02290
C                                                                        LN02291
C     CALL VECISO TO LOAD ISOTOPE INFORMATION INTO /ISVECT/              LN02292
C                                                                        LN02293
      CALL VECISO                                                        LN02294
C                                                                        LN02295
      IFIL1 = 1                                                          LN02300
      IFIL2 = 2                                                          LN02310
      LINFIL = 3                                                         LN02320
      OPEN (LINFIL,FILE='TAPE3',STATUS='NEW',FORM=CFORM)                 LN02330
      OPEN (IRD,FILE='TAPE5',STATUS='UNKNOWN',FORM='FORMATTED')          LN02340
      OPEN (IPR,FILE='TAPE6',STATUS='UNKNOWN',FORM='FORMATTED')          LN02350
      LINMRG = 10                                                        LN02360
      OPEN (LINMRG,FILE='TAPE10',STATUS='UNKNOWN',FORM=CFORM)            LN02370
      ILIN = 0                                                           LN02380
      NWDS = 0                                                           LN02390
C                                                                        LN02400
      READ (IRD,900) (HID(I),I=1,9)                                      LN02410
C                                                                        LN02420
C     HID CONTAINS 72 CHARACTERS OF HEADER IDENTIFICATION.               LN02430
C                                                                        LN02440
C     CHID08 CONTAINS THE SCCS VERSION NUMBER OF LNFL.F, AND
C     CHID10 CONTAINS THE FLAG "I" FOR THE ISOTOPE INCLUSION
C     IN TAPE3, TESTED IN LBLRTM
C
      READ (CHID08,900) HID(8)                                           LN02450
      READ (CHID10,900) HID(10)                                          LN02450
      CALL LBLDAT (HDATE)                                                LN02460
      CALL FTIME (HTIME)                                                 LN02470
      WRITE (IPR,905) HID,HID1                                           LN02480
C                                                                        LN02490
      READ (IRD,910) VMIN,VMAX                                           LN02500
      WRITE (IPR,915) VMIN,VMAX                                          LN02510
C                                                                        LN02520
C     VMIN IS LOW WAVENUMBER LIMIT; VMAX IS HIGH WAVENUMBER LIMIT.       LN02530
C                                                                        LN02540
      READ (IRD,920) (MIND1(I),I=1,32),HOLIND                            LN02550
C                                                                        LN02560
C     INBLK1 AND INBLK2 ARE FLAGS TO DETERMINE THE BLOCKING FOR          LN02570
C     INPUT FILES TAPE1 AND TAPE2                                        LN02580
C                                                                        LN02590
C     FOR INBLCK  = 0, BLOCKING = 51 TRANSITIONS/BLOCK   100 CHAR./TRAN  LN02600
C     FOR INBLCK  = 1, BLOCKING =  1 TRANSITIONS/BLOCK   100 CHAR./TRAN  LN02610
C     FOR IPUOUT = 1, THE MERGED HITRAN TAPE IS OUTPUT TO TAPE7          LN02620
C                                                                        LN02630
      INBLK1 = 0                                                         LN02640
      INBLK2 = 0                                                         LN02650
      IPUOUT = 0                                                         LN02660
      CALL HOLRT (5,HNBLK1,INBLK1,HOLIND,40)                             LN02670
      IF (INBLK1.EQ.1) WRITE (IPR,925) HNBLK1                            LN02680
      CALL HOLRT (5,HNBLK2,INBLK2,HOLIND,40)                             LN02690
      IF (INBLK2.EQ.1) WRITE (IPR,925) HNBLK2                            LN02700
      CALL HOLRT (5,HLNOUT,IPUOUT,HOLIND,40)                             LN02710
      IF (IPUOUT.EQ.1) WRITE (IPR,925) HLNOUT                            LN02720
C                                                                        LN02730
      IF (IPUOUT.EQ.1)                                                   LN02740
     *     OPEN (IPU,FILE='TAPE7',STATUS='UNKNOWN',FORM='FORMATTED')     LN02750
C                                                                        LN02760
C     FOR TAPE2
C
      CALL HOLRT (3,HF100,IF100,HOLIND,40)                               LN02770
      CALL HOLRT (3,HF80,IF80,HOLIND,40)                                 LN02780
      CALL HOLRT (5,HNOCPL,INCPST,HOLIND,40)                             LN02790
C
C     FOR HITRAN86
C
      CALL HOLRT (5,HH86T1,I86T1,HOLIND,40)
      IF (I86T1.EQ.1) WRITE (IPR,925) HH86T1
      CALL HOLRT (5,HH86T2,I86T2,HOLIND,40)
      IF (I86T2.EQ.1) WRITE (IPR,925) HH86T2
C                                                                        LN02800
      OPEN (IFIL1,FILE='TAPE1',STATUS='OLD',FORM='FORMATTED')            LN02810
c     skip over first 102 records in header of hitran 86
      do 5 i=1,102
         read(ifil1,901) cdum
 5    continue
C                                                                        LN02820
      IF (IF80.EQ.1) IMRG2 = 1                                           LN02830
      IF (IF80.EQ.1.OR.IF100.EQ.1) INCPST = 1                            LN02840
C                                                                        LN02850
      IF (INCPST.NE.1) THEN                                              LN02860
           OPEN (IFIL2,FILE='TAPE2',STATUS='NEW',FORM='FORMATTED')       LN02870
      ELSE                                                               LN02880
         IF (IF80.EQ.1.OR.IF100.EQ.1)                                    LN02890
     *        OPEN (IFIL2,FILE='TAPE2',STATUS='OLD',FORM='FORMATTED')    LN02900
      ENDIF                                                              LN02910
C                                                                        LN02920
      IF (INCPST.NE.1.OR.IF100.EQ.1) THEN                                LN02930
         IMRG2 = -1                                                      LN02940
         IF (INCPST.NE.1) THEN                                           LN02950
            CALL CPSTOR                                                  LN02960
            INBLK2 = 1                                                   LN02970
         ELSE                                                            LN02980
            WRITE (IPR,925) HNOCPL                                       LN02990
         ENDIF                                                           LN03000
      ENDIF                                                              LN03010
      IF (ABS(IMRG2).EQ.1) THEN                                          LN03020
         IF (IMRG2.EQ.1) THEN                                            LN03030
C                                                                        LN03040
            READ (IRD,920) (MIND2(I),I=1,32)                             LN03050
            WRITE (IPR,930) HF80                                         LN03060
         ELSE                                                            LN03070
C                                                                        LN03080
            IF (IF100.EQ.1) THEN                                         LN03090
               READ (IRD,920) (MINDC(I),I=1,32)                          LN03100
               WRITE (IPR,930) HF100                                     LN03110
            ELSE                                                         LN03120
               DO 10 I = 1, 30                                           LN03130
                  MINDC(I) = MOLCPL(I)                                   LN03140
                  IF (MIND1(I).EQ.0) MINDC(I) = 0                        LN03150
   10          CONTINUE                                                  LN03160
            ENDIF                                                        LN03170
         ENDIF                                                           LN03180
      ENDIF                                                              LN03190
      CALL HOLRT (4,HNLTE,INLTE,HOLIND,40)                               LN03200
      IF (INLTE.EQ.1) THEN                                               LN03210
         READ (GNLTE,900) HID(9)                                         LN03220
         WRITE (IPR,930) HNLTE                                           LN03230
      ENDIF                                                              LN03240
      CALL HOLRT (3,HREJ,IREJ,HOLIND,40)                                 LN03250
      IF (IREJ.EQ.1) WRITE (IPR,930) HREJ                                LN03260
C                                                                        LN03270
C     MOLIND IS AN ARRAY TO SELECT MOLECULES (=1 YES, =0 NO), FORMAT     LN03280
C     (32I1)        -- PUT 1 IN COLUMN CORRESPONDING TO MOLECULE ID.     LN03290
C                                                                        LN03300
      DO 20 I = 1, 32                                                    LN03310
         READ (CMOL(I),935) HMOL(I)                                      LN03320
         IF (MIND1(I).GT.0) MOLIND(I) = 1                                LN03330
         IF (MIND2(I).GT.0) MOLIND(I) = MOLIND(I)+2                      LN03340
         IF (MINDC(I).GT.0.AND.IF100.EQ.1) MOLIND(I) = MOLIND(I)+2       LN03350
         IF (MOLIND(I).GE.3) PRINT 940,I                                 LN03360
         IF (MOLIND(I).NE.0) NMOL = I                                    LN03370
C                                                                        LN03380
   20 CONTINUE                                                           LN03390
C                                                                        LN03400
      NMOL = MAX(NMOL,7)                                                 LN03410
      WRITE (IPR,945) (HMOL(I),MOLIND(I),I=1,NMOL)                       LN03420
      WRITE (IPR,945)                                                    LN03430
      IF (IREJ.EQ.1) THEN                                                LN03440
         READ (GREJ,900) HID(9)                                          LN03450
         IF (INLTE.EQ.1) READ (GREJNL,900) HID(9)                        LN03460
         READ (IRD,950) (SR(I),I=1,NMOL)                                 LN03470
      ENDIF                                                              LN03480
C                                                                        LN03490
C     IF STRENGTH REJECTION READ IN IS NEGATIVE, SET TO DEFAULT VALUE    LN03500
C                                                                        LN03510
      DO 30 I = 1, NMOL                                                  LN03520
         IF (IREJ.EQ.1.AND.SR(I).LT.0) SR(I) = SRD(I)                    LN03530
   30 CONTINUE                                                           LN03540
C                                                                        LN03550
C     IF HITRAN TAPE, SKIP OVER RECORDS NOT NEEDED                       LN03560
C                                                                        LN03570
      IEOF1 = 0                                                          LN03580
      IEOF2 = 2                                                          LN03590
      IF (INBLK1.EQ.0) CALL SKIPT (IFIL1,VMIN,IEOF1)                     LN03600
      IF (INBLK2.EQ.0.AND.IMRG2.EQ.-1) CALL SKIPT (IFIL2,VMIN,IEOF2)     LN03610
C                                                                        LN03620
C     IF VMIN .LE. .007 AND O2 IS INCLUDED, SET FLAG TO REPLACE          LN03630
C     O2 LINES WITH SPECIAL BAND FOR LINE COUPLING CASE                  LN03640
C                                                                        LN03650
      IO2BND = 0                                                         LN03660
      IF (VMIN.LE..007.AND.IMRG2.EQ.-1.AND.MINDC(7).NE.0) IO2BND = 1     LN03670
C                                                                        LN03680
      IF (IMRG2.EQ.1) CALL RDFL82 (IFIL2,I2,N2,IEOF2,VLST2)              LN03690
      IF (IMRG2.EQ.-1) CALL RDFIL2 (IFIL2,I2,N2,IEOF2)                   LN03700
      IF (IO2BND.EQ.-1) WRITE (IPR,955)                                  LN03710
C                                                                        LN03720
   40 IF (IEOF1.EQ.0) THEN                                               LN03730
         CALL RDFIL1 (IFIL1,I1,N1,IEOF1)                                 LN03740
      ELSE                                                               LN03750
         N1 = 0                                                          LN03760
      ENDIF                                                              LN03770
      IF (N1.LT.1) GO TO 90                                              LN03780
C                                                                        LN03790
      DO 80 J = I1, N1                                                   LN03800
         IREPLN = 0                                                      LN03810
         I = IOUT(J)                                                     LN03820
         IF (IMRG2.EQ.-1.AND.I2.GE.1) IC = IOUTC(I2)                     LN03830
         IF (MOL1(I).EQ.0) GO TO 80                                      LN03840
   50    IF (N2.LT.1) GO TO 70                                           LN03850
         IF (IMRG2.EQ.1) THEN                                            LN03860
            IF (MOL2(I2).EQ.0) GO TO 60                                  LN03870
            IF (VNU1(I).LE.VNU2(I2)) GO TO 70                            LN03880
            CALL MOVE (LINMRG,I2,VNU2,STR2,ALF2,EPP2,MOL2,AMOL2,HWHM2,   LN03890
     *                 TMPAL2,PSHIF2,IFG2,MOLCNT,ALIN2)                  LN03900
         ENDIF                                                           LN03910
         IF (IMRG2.EQ.-1) THEN                                           LN03920
            IF (MOLC(IC).EQ.0) GO TO 60                                  LN03930
C                                                                        LN03940
C     CHECK FOR O2 MOLECULE IF VNU LT .007 TO REPLACE                    LN03950
C     LINE WITH A BAND CENTERED AT .000010 CM-1                          LN03960
C                                                                        LN03970
            IF (IO2BND.EQ.-1.AND.VNU1(I).LT..007) THEN                   LN03980
               MMOL1 = MOD(MOL1(I),100)                                  LN03990
               IF (MMOL1.EQ.7.AND.IFG1(I).NE.3) GO TO 80                 LN04000
            ENDIF                                                        LN04010
            VNUTST = VNUC(IC)-VNU1(I)                                    LN04020
C                                                                        LN04030
C    CHECK TO SEE IF WAVENUMBERS ARE WITHIN  1.0 WAVENUMBERS             LN04040
C    IF SO, CHECK FOR MATCH OF QUANTUM NUMBERS - IF MATCH                LN04050
C    REPLACE LINE IN FILE1 WITH LINE IN FILE2                            LN04060
C                                                                        LN04070
            IF (ABS(VNUTST).LE.1.) THEN                                  LN04080
               IF (QUANTC(IC).EQ.QUANT1(I)) THEN                         LN04090
                  CALL MOVE (LINMRG,IC,VNUC,STRC,ALFC,EPPC,MOLC,AMOLC,   LN04100
     *                       HWHMC,TMPALC,PSHIFC,IFGC,MOLCNT,ALINC)      LN04110
                  IREPLN = 1                                             LN04120
                  GO TO 60                                               LN04130
               ENDIF                                                     LN04140
            ENDIF                                                        LN04150
            IF (VNU1(I).LE.VNUC(IC)) GO TO 70                            LN04160
            CALL MOVE (LINMRG,IC,VNUC,STRC,ALFC,EPPC,MOLC,AMOLC,HWHMC,   LN04170
     *                 TMPALC,PSHIFC,IFGC,MOLCNT,ALINC)                  LN04180
         ENDIF                                                           LN04190
   60    I2 = I2+1                                                       LN04200
         IF (IMRG2.EQ.-1.AND.I2.GE.1) IC = IOUTC(I2)                     LN04210
         IF (I2.LE.N2) THEN                                              LN04220
            IF (IREPLN.EQ.1) GO TO 80                                    LN04230
            GO TO 50                                                     LN04240
         ENDIF                                                           LN04250
         IF (IEOF2.EQ.0.AND.IMRG2.NE.0) THEN                             LN04260
            IF (IMRG2.EQ.1) CALL RDFL82 (IFIL2,I2,N2,IEOF2,VLST2)        LN04270
            IF (IMRG2.EQ.-1) CALL RDFIL2 (IFIL2,I2,N2,IEOF2)             LN04280
         ELSE                                                            LN04290
            N2 = 0                                                       LN04300
         ENDIF                                                           LN04310
         IF (IMRG2.EQ.0) IEOF2 = 1                                       LN04320
         IF (IMRG2.EQ.-1.AND.I2.GE.1) IC = IOUTC(I2)                     LN04330
         IF (IREPLN.EQ.1) GO TO 80                                       LN04340
         GO TO 50                                                        LN04350
   70    CONTINUE                                                        LN04360
         CALL MOVE (LINMRG,I,VNU1,STR1,ALF1,EPP1,MOL1,AMOL1,HWHM1,       LN04370
     *              TMPAL1,PSHIF1,IFG1,MOLCNT,ALIN1)                     LN04380
   80 CONTINUE                                                           LN04390
      GO TO 40                                                           LN04400
   90 IF (N2.LT.0) GO TO 110                                             LN04410
      IF (I2.GT.N2) GO TO 110                                            LN04420
      IF (I2.EQ.0) GO TO 100                                             LN04430
      IF (IMRG2.EQ.1) THEN                                               LN04440
         CALL MOVE (LINMRG,I2,VNU2,STR2,ALF2,EPP2,MOL2,AMOL2,HWHM2,      LN04450
     *              TMPAL2,PSHIF2,IFG2,MOLCNT,ALIN2)                     LN04460
      ELSEIF (IMRG2.EQ.-1) THEN                                          LN04470
         IC = IOUTC(I2)                                                  LN04480
         CALL MOVE (LINMRG,IC,VNUC,STRC,ALFC,EPPC,MOLC,AMOLC,HWHMC,      LN04490
     *              TMPALC,PSHIFC,IFGC,MOLCNT,ALINC)                     LN04500
      ENDIF                                                              LN04510
  100 I2 = I2+1                                                          LN04520
      IF (IMRG2.EQ.-1.AND.I2.GE.1) IC = IOUTC(I2)                        LN04530
      IF (I2.LE.N2) GO TO 90                                             LN04540
      IF (IEOF2.EQ.0.AND.IMRG2.NE.0) THEN                                LN04550
         IF (IMRG2.EQ.1) CALL RDFL82 (IFIL2,I2,N2,IEOF2,VLST2)           LN04560
         IF (IMRG2.EQ.-1) CALL RDFIL2 (IFIL2,I2,N2,IEOF2)                LN04570
      ELSE                                                               LN04580
         N2 = 0                                                          LN04590
      ENDIF                                                              LN04600
      IF (IMRG2.EQ.-1.AND.I2.GE.1) IC = IOUTC(I2)                        LN04610
      IF (IMRG2.EQ.0) IEOF2 = 1                                          LN04620
      GO TO 90                                                           LN04630
C                                                                        LN04640
  110 CALL BLKOUT (LINMRG)                                               LN04650
      REWIND LINMRG                                                      LN04660
      REWIND LINFIL                                                      LN04670
      DO 120 M = 1, NMOL                                                 LN04680
         MOLIND(M) = MOLCNT(M)                                           LN04690
  120 CONTINUE                                                           LN04700
      WRITE (IPR,960) LINFIL,FLINLO,FLINHI,ILIN                          LN04710
      WRITE (IPR,965) (HMOL(I),MOLCNT(I),MCNTLC(I),MCNTNL(I),SUMSTR(I),  LN04720
     *                 SR(I),I=1,NMOL)                                   LN04730
      CALL BUFOUT (LINFIL,HID(1),NWDLIN)                                 LN04740
      WRITE (IPR,970) NBLOCK                                             LN04750
C                                                                        LN04760
      DO 130 I = 1, NBLOCK                                               LN04770
         CALL BUFIN (LINMRG,IEOF,RCDHDR(1),LRC)                          LN04780
         NWDS = ILNGTH                                                   LN04790
         CALL BUFOUT (LINFIL,RCDHDR(1),LRC)                              LN04800
         CALL BUFIN (LINMRG,IEOF,VNU3(1),ILNGTH)                         LN04810
         CALL CKFL (VLO,VHI,LINES,VNU3,IFLG)                             LN04820
         CALL BUFOUT (LINFIL,VNU3(1),ILNGTH)                             LN04830
  130 CONTINUE                                                           LN04840
C                                                                        LN04850
      CALL CPUTIM (TIME1)                                                LN04860
      TIME = TIME1-TIME0                                                 LN04870
      WRITE (IPR,975) TIME,TIME0,TIME1                                   LN04880
C                                                                        LN04890
      STOP ' LINFIL COMPLETE '                                           LN04900
C                                                                        LN04910
  900 FORMAT (10A8)                                                      LN04920
 901  format (a1)
  905 FORMAT ('1',10A8,2(1X,A8,1X))                                      LN04930
  910 FORMAT (2F10.3)                                                    LN04940
  915 FORMAT ('0',20X,'VMIN =',F12.6,' CM-1,      VMAX =',F12.6,         LN04950
     *        ' CM-1')                                                   LN04960
  920 FORMAT (32I1,8X,A40)                                               LN04970
  925 FORMAT ('0',' ** NOTE IFLG SET - ',A5,' *****',/)                  LN04980
  930 FORMAT ('0',' ** NOTE IFLG SET - ',A4,' *****',/)                  LN04990
  935 FORMAT (A6)                                                        LN05000
  940 FORMAT (' YOU HAVE ASKED FOR THE SAME MOLECULE ON  TWO TAPES',/,   LN05010
     *        '      DUPLICATE LINES MAY ARISE.  MOLECULE = ',I3)        LN05020
  945 FORMAT (' ',40X,A6,' = ',I1)                                       LN05030
  950 FORMAT (8E10.3)                                                    LN05040
  955 FORMAT ('0',' O2 LINES < .007 CM-1 HAVE BEEN REPLACED BY A O2',    LN05050
     *        1X,'BAND CENTERED AT .000010 CM-1 ',/)                     LN05060
  960 FORMAT ('0',9X,'TAPE NO. =',I2/10X,'LOWEST LINE =',F12.6,          LN05070
     *        ' CM-1,  ','HIGHEST LINE =',F12.6,                         LN05080
     *        ' CM-1,  TOTAL NUMBER OF LINES =',I7)                      LN05090
  965 FORMAT ('0',/,23X,'COUPLED',4X,'NLTE',3X,'SUM LBLRTM',6X,          LN05100
     *        'STRENGTH',/,7X,'MOL',5X,'LINES',4X,'LINES',4X,'LINES',    LN05110
     *        4X,'STRENGTHS',6X,'REJECTION',2(/),(' ',4X,A6,' = ',I6,    LN05120
     *        3X,I6,3X,I6,2X,1PE12.4,4X,E10.3,0P))                       LN05130
  970 FORMAT ('0',20X,'NUMBER OF BLOCKS =',I4)                           LN05140
  975 FORMAT ('0',10X,' TOTAL TIME =',F10.3,' TIME IN =',F10.3,          LN05150
     *        ' TIME OUT =',F10.3)                                       LN05160
C                                                                        LN05170
      END                                                                LN05180
      FUNCTION NWDL (IWD,ILAST)                                          LN06490
C                                                                        LN06500
      DIMENSION IWD(*)                                                   LN06510
C                                                                        LN06520
      ILAST = -654321                                                    LN06530
      DO 10 I = 1, 9000                                                  LN06540
         IF (IWD(I).EQ.ILAST) THEN                                       LN06550
            NWDL = I-1                                                   LN06560
            RETURN                                                       LN06570
         ENDIF                                                           LN06580
   10 CONTINUE                                                           LN06590
C                                                                        LN06600
      STOP ' NWDL - IWD,ILAST '                                          LN06610
C                                                                        LN06620
      END                                                                LN06630
      SUBROUTINE HOLRT (N,HOL,IHOL,HOLIND,NMAX)                          LN06640
C                                                                        LN06650
      CHARACTER HOL*(*),HOLIND*40,BLNK*1                                 LN06660
C                                                                        LN06670
      DATA BLNK / ' '/                                                   LN06680
C                                                                        LN06690
      IHOL = 0                                                           LN06700
      J = N-1                                                            LN06710
      IMAX = NMAX-J                                                      LN06720
      DO 10 I = 1, IMAX                                                  LN06730
         IF (HOLIND(I:I+J).EQ.HOL(1:N)) THEN                             LN06740
            IHOL = 1                                                     LN06750
            RETURN                                                       LN06760
         ENDIF                                                           LN06770
   10 CONTINUE                                                           LN06780
C                                                                        LN06790
      RETURN                                                             LN06800
C                                                                        LN06810
      END                                                                LN06820
      SUBROUTINE CKFL (VLO,VHI,LINES,VNU3,IFLG)                          LN06830
C                                                                        LN06840
      IMPLICIT DOUBLE PRECISION (V)                                     !LN06850
C                                                                        LN06860
      DIMENSION VNU3(250),IFLG(250)                                      LN06870
C                                                                        LN06880
      VST = VLO                                                          LN06890
      DO 10 I = 1, LINES                                                 LN06900
         IF (IFLG(I).GE.0) THEN                                          LN06910
            IF (VNU3(I).LT.VST) PRINT 900,I,VST,VNU3(I)                  LN06920
            VST = VNU3(I)                                                LN06930
            IF (VNU3(I).GT.VHI) PRINT 905,I,VHI,VNU3(I)                  LN06940
         ENDIF                                                           LN06950
   10 CONTINUE                                                           LN06960
C                                                                        LN06970
      RETURN                                                             LN06980
C                                                                        LN06990
  900 FORMAT (' * CKFL * VNU LT VST: I = ',I5,' VST = ',F15.5,           LN07000
     *        ' VNU(I) = ',F15.5)                                        LN07010
  905 FORMAT (' * CKFL * VNU GT VHI: I = ',I5,' VHI = ',F15.5,           LN07020
     *        ' VNU(I) = ',F15.5)                                        LN07030
C                                                                        LN07040
      END                                                                LN07050
      SUBROUTINE SKIPT (IFIL,VNU,IEOF)                                   LN07060
C                                                                        LN07070
      IMPLICIT DOUBLE PRECISION (V)                                     !LN07080
C                                                                        LN07090
   10 READ (IFIL,900,END=20) VA                                          LN07100
C                                                                        LN07110
C#    IF (UNIT(IFIL) .EQ. 0.) IEOF=0                                     LN07120
C                                                                        LN07130
      IEOF = 0                                                           LN07140
      IF (VA.LT.VNU) GO TO 10                                            LN07150
      BACKSPACE IFIL                                                     LN07160
      BACKSPACE IFIL                                                     LN07170
C                                                                        LN07180
      RETURN                                                             LN07190
C                                                                        LN07200
   20 PRINT 905,IFIL,VNU                                                 LN07210
      IEOF = 1                                                           LN07220
C                                                                        LN07230
      RETURN                                                             LN07240
C                                                                        LN07250
  900 FORMAT (3X,F12.6)                                                  LN07260
  905 FORMAT (' ERROR HIT EOF ON TAPE ',I2,' BEFORE ',F10.2)             LN07270
C                                                                        LN07280
      END                                                                LN07290
      SUBROUTINE ISTOPE (NSO82,ISO85,MOLEC,ICHOIC)                       LN07300
C                                                                        LN07310
C     ICHOIC = 1 :FIND 1985 VECTOR CODE FROM 1982 CODE                   LN07320
C     ICHOIC = 2 :FIND 1982 CODE FROM 1985 VECTOR CODE                   LN07330
C                                                                        LN07340
      PARAMETER (NTMOL=32,NSPECI=75)                                     LN07350
C                                                                        LN07360
      COMMON /ISVECT/ ISOVEC(NTMOL),ISO82(NSPECI),ISONM(NTMOL)           LN07370
      COMMON /FDES/ IU1,IU2,IU3,IU4,IU9,IU77                             LN07380
C                                                                        LN07390
      IF (ICHOIC.EQ.1) THEN                                              LN07400
C                                                                        LN07410
C     FIND LOCATION TO SEARCH:                                           LN07420
C                                                                        LN07430
         NS = ISOVEC(MOLEC)+1                                            LN07440
         NEND = ISONM(MOLEC)+ISOVEC(MOLEC)                               LN07450
C                                                                        LN07460
         DO 10 I = NS, NEND                                              LN07470
            IF (NSO82.EQ.ISO82(I)) THEN                                  LN07480
               ISO85 = I-ISOVEC(MOLEC)                                   LN07490
               RETURN                                                    LN07500
            ENDIF                                                        LN07510
   10    CONTINUE                                                        LN07520
C                                                                        LN07530
      ELSE                                                               LN07540
         NSO82 = ISO82(ISOVEC(MOLEC)+ISO85)                              LN07550
      ENDIF                                                              LN07560
C                                                                        LN07570
      RETURN                                                             LN07580
C                                                                        LN07590
      END                                                                LN07600
      SUBROUTINE VECISO                                                  LN07610
C                                                                        LN07620
      PARAMETER (NTMOL=32,NSPECI=75)                                     LN07630
C                                                                        LN07640
      COMMON /ISVECT/ ISOVEC(NTMOL),ISO82(NSPECI),ISONM(NTMOL)           LN07650
C                                                                        LN07660
C     ISOTOPE VECTOR INFORMATION                                         LN07670
C        SET UP ISOVEC:                                                  LN07680
C                                                                        LN07690
      ISOVEC(1) = 0                                                      LN07700
      DO 20 I = 2, NTMOL                                                 LN07710
         ISOVEC(I) = 0                                                   LN07720
         DO 10 J = 1, I-1                                                LN07730
            ISOVEC(I) = ISOVEC(I)+ISONM(J)                               LN07740
   10    CONTINUE                                                        LN07750
   20 CONTINUE                                                           LN07760
C                                                                        LN07770
      RETURN                                                             LN07780
C                                                                        LN07790
      END                                                                LN07800
      BLOCK DATA ISOTPE                                                  LN07810
C                                                                        LN07820
      PARAMETER (NTMOL=32,NSPECI=75)                                     LN07830
C                                                                        LN07840
      COMMON /ISVECT/ ISOVEC(NTMOL),ISO82(NSPECI),ISONM(NTMOL)           LN07850
C                                                                        LN07860
C    THE NUMBER OF ISOTOPES FOR A PARTICULAR MOLECULE:                   LN07870
C                                                                        LN07880
      DATA (ISONM(I),I=1,NTMOL)/                                         LN07890
C                                                                        LN07900
C     H2O, CO2, O3, N2O, CO, CH4, O2,                                    LN07910
C                                                                        LN07920
     *  4,   8,  3,   5,  5,   3,  3,                                    LN07930
C                                                                        LN07940
C      NO, SO2, NO2, NH3, HNO3, OH, HF, HCL, HBR, HI,                    LN07950
C                                                                        LN07960
     *  3,   2,   1,   2,    1,  3,  1,   2,   2,  1,                    LN07970
C                                                                        LN07980
C     CLO, OCS, H2CO, HOCL, N2, HCN, CH3CL, H2O2, C2H2, C2H6, PH3        LN07990
C                                                                        LN08000
     *  2,   4,    3,    2,  1,   3,     2,    1,    2,    1,   1,       LN08010
C                                                                        LN08020
C     COF2, SF6, H2S, HCOOH                                              LN08030
C                                                                        LN08040
     *   1,   1,   1,     1 /                                            LN08050
C                                                                        LN08060
      DATA ISO82/                                                        LN08070
C                                                                        LN08080
C       H2O                                                              LN08090
C                                                                        LN08100
     *  161,181,171,162,                                                 LN08110
C                                                                        LN08120
C       CO2                                                              LN08130
C                                                                        LN08140
     *  626,636,628,627,638,637,828,728,                                 LN08150
C                                                                        LN08160
C       O3                                                               LN08170
C                                                                        LN08180
     *  666,668,686,                                                     LN08190
C                                                                        LN08200
C       N2O                                                              LN08210
C                                                                        LN08220
     *  446,456,546,448,447,                                             LN08230
C                                                                        LN08240
C       CO,              CH4                                             LN08250
C                                                                        LN08260
     *  26,36,28,27,38,  211,311,212,                                    LN08270
C                                                                        LN08280
C       O2,        NO,        SO2                                        LN08290
C                                                                        LN08300
     *  66,68,67,  46,56,48  ,626,646,                                   LN08310
C                                                                        LN08320
C       NO2,   NH3,        HNO3                                          LN08330
C                                                                        LN08340
     *  646,   4111,5111,  146,                                          LN08350
C                                                                        LN08360
C       OH,        HF,  HCL,    HBR,    HI                               LN08370
C                                                                        LN08380
     *  61,81,62,  19,  15,17,  19,11,  17,                              LN08390
C                                                                        LN08400
C       CLO,    OCS,              H2CO                                   LN08410
C                                                                        LN08420
     *  56,76,  622,624,632,822,  126,136,128,                           LN08430
C                                                                        LN08440
C       HOCL,     N2,  HCN                                               LN08450
C                                                                        LN08460
     *  165,167,  44,  124,134,125,                                      LN08470
C                                                                        LN08480
C       CH3CL,    H2O2,  C2H2,       C2H6,  PH3                          LN08490
C                                                                        LN08500
     *  215,217,  1661,  1221,1231,  1221,  1111,                        LN08510
C                                                                        LN08520
C       COF2, SF6, H2S, HCOOH                                            LN08530
C                                                                        LN08540
     *  269,  29, 121,   126/                                            LN08550
C                                                                        LN08560
      END                                                                LN08570
      SUBROUTINE MOVE (LINMRG,I,VNU2,STR2,ALF2,EPP2,MOL2,AMOL2,HWHM2,    LN08580
     *                 TMPAL2,PSHIF2,IFG2,MOLCNT,ALIN2)                  LN08590
C                                                                        LN08600
      IMPLICIT DOUBLE PRECISION (V)                                     !LN08610
C                                                                        LN08620
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN08630
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN08640
     *               IREC,IRECTL,HID1(2),LSTWD                           LN08650
      COMMON VNU3(250),STR3(250),ALF3(250),EPP3(250),MOL3(250),          LN08660
     *       HWHMS(250),TMPALF(250),PSHIFT(250),IFLG(250),LSTW2          LN08670
C                                                                        LN08680
      DOUBLE PRECISION HID,HID1,HMOL                                    &LN08690
C                                                                        LN08700
      DIMENSION VNU2(*),STR2(*),ALF2(*),EPP2(*),MOL2(*),AMOL2(*),        LN08710
     *          HWHM2(*),TMPAL2(*),PSHIF2(*),IFG2(*),ALIN2(*)            LN08720
      DIMENSION MOLCNT(*)                                                LN08730
      COMMON /ICN/ ILIN3,NMAX,NBLOCK                                     LN08740
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN08750
      COMMON /LCHAR/ ALIN1(51),ALINE(40),ALINC(51),ALIN(250)             LN08760
C                                                                        LN08770
      CHARACTER*100 ALIN1,ALIN2,ALINC,ALIN,ALINE                         LN08780
      DIMENSION AMOL3(250)                                               LN08790
C                                                                        LN08800
      EQUIVALENCE (AMOL3(1),MOL3(1))                                     LN08810
C                                                                        LN08820
      M = MOD(MOL2(I),100)                                               LN08830
      ILIN3 = ILIN3+1                                                    LN08840
      NWDS = NWDS+9                                                      LN08850
      VNU3(ILIN3) = VNU2(I)                                              LN08860
      STR3(ILIN3) = STR2(I)                                              LN08870
      ALF3(ILIN3) = ALF2(I)                                              LN08880
      EPP3(ILIN3) = EPP2(I)                                              LN08890
      MOL3(ILIN3) = MOL2(I)                                              LN08900
      HWHMS(ILIN3) = HWHM2(I)                                            LN08910
      TMPALF(ILIN3) = TMPAL2(I)                                          LN08920
      PSHIFT(ILIN3) = PSHIF2(I)                                          LN08930
      IFLG(ILIN3) = IFG2(I)                                              LN08940
      ALIN(ILIN3) = ALIN2(I)                                             LN08950
      ILIN = ILIN+1                                                      LN08960
      SUMSTR(M) = SUMSTR(M)+STR2(I)                                      LN08970
      MOLCNT(M) = MOLCNT(M)+1                                            LN08980
      IF (IFG2(I).GE.1) THEN                                             LN08990
         NWDS = NWDS+9                                                   LN09000
         ILIN3 = ILIN3+1                                                 LN09010
         VNU3(ILIN3) = VNU2(I+1)                                         LN09020
         STR3(ILIN3) = STR2(I+1)                                         LN09030
         ALF3(ILIN3) = ALF2(I+1)                                         LN09040
         EPP3(ILIN3) = EPP2(I+1)                                         LN09050
         AMOL3(ILIN3) = AMOL2(I+1)                                       LN09060
         HWHMS(ILIN3) = HWHM2(I+1)                                       LN09070
         TMPALF(ILIN3) = TMPAL2(I+1)                                     LN09080
         PSHIFT(ILIN3) = PSHIF2(I+1)                                     LN09090
         IFLG(ILIN3) = IFG2(I+1)                                         LN09100
         ALIN(ILIN3) = ALIN2(I+1)                                        LN09110
         MCNTLC(M) = MCNTLC(M)+1                                         LN09120
         ILINLC = ILINLC+1                                               LN09130
      ENDIF                                                              LN09140
      MIS = MOD(MOL2(I),1000)                                            LN09143
      IF (MIS.NE.MOL2(I)) THEN                                           LN09146
         MCNTNL(M) = MCNTNL(M)+1                                         LN09150
         ILINNL = ILINNL+1                                               LN09160
      ENDIF                                                              LN09165
      IF (ILIN3.GE.NMAX-1) CALL BLKOUT (LINMRG)                          LN09170
C                                                                        LN09180
      RETURN                                                             LN09190
C                                                                        LN09200
      END                                                                LN09210
      SUBROUTINE BLKOUT (LINMRG)                                         LN09220
C                                                                        LN09230
      IMPLICIT DOUBLE PRECISION (V)                                     !LN09240
C                                                                        LN09250
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN09260
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN09270
     *               IREC,IRECTL,HID1(2),LSTWD                           LN09280
C                                                                        LN09290
      DOUBLE PRECISION HID,HID1,HMOL                                    &LN09300
C                                                                        LN09310
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN09320
      COMMON VNU3(250),STR3(250),ALF3(250),EPP3(250),MOL3(250),          LN09330
     *       HWHMS(250),TMPALF(250),PSHIFT(250),IFLG(250),LSTW2          LN09340
      COMMON /LCHAR/ ALIN1(51),ALIN2(40),ALINC(51),ALIN(250)             LN09350
      CHARACTER*100 ALIN1,ALIN2,ALINC,ALIN,ALINE                         LN09360
      COMMON /ICN/ I3,NMAX,NBLOCK                                        LN09370
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN09380
      DIMENSION RCDHDR(5)                                                LN09390
C                                                                        LN09400
      EQUIVALENCE (RCDHDR(1),VLO)                                        LN09410
      CHARACTER*50 ZEROFL                                                LN09420
C                                                                        LN09430
      DATA ZEROFL /                                                      LN09440
     *           '00000000000000000000000000000000000000000000000000'/   LN09450
C                                                                        LN09460
      NBLOCK = NBLOCK+1                                                  LN09470
      WRITE (ALINE,900) ZEROFL,ZEROFL                                    LN09480
      IF (I3.EQ.NMAX) GO TO 20                                           LN09490
      I1 = I3+1                                                          LN09500
      IF (I1.GT.NMAX) GO TO 20                                           LN09510
      DO 10 I = I1, NMAX                                                 LN09520
         NWDS = NWDS+9                                                   LN09530
         VNU3(I) = 0.                                                    LN09540
         STR3(I) = 0.                                                    LN09550
         ALF3(I) = 0.                                                    LN09560
         EPP3(I) = 0.                                                    LN09570
         MOL3(I) = 0.                                                    LN09580
         HWHMS(I) = 0.                                                   LN09590
         TMPALF(I) = 0.                                                  LN09600
         IFLG(I) = 0                                                     LN09610
   10 CONTINUE                                                           LN09620
   20 VLO = VNU3(1)                                                      LN09630
      ITST = I3                                                          LN09640
   30 IF (IFLG(ITST).GE.0) THEN                                          LN09650
         VHI = VNU3(ITST)                                                LN09660
      ELSE                                                               LN09670
         ITST = ITST-1                                                   LN09680
         GO TO 30                                                        LN09690
      ENDIF                                                              LN09700
      IF (NBLOCK.EQ.1) FLINLO = VLO                                      LN09710
      FLINHI = VHI                                                       LN09720
      LINES = I3                                                         LN09730
      CALL BUFOUT (LINMRG,RCDHDR(1),LRC)                                 LN09740
      CALL BUFOUT (LINMRG,VNU3(1),ILNGTH)                                LN09750
      IF (IPUOUT.NE.1) GO TO 50                                          LN09760
      DO 40 IOUT = 1, I3                                                 LN09770
         IF (ALIN(IOUT).EQ.ALINE) GO TO 40                               LN09780
         WRITE (IPU,905) ALIN(IOUT)                                      LN09790
   40 CONTINUE                                                           LN09800
   50 NWDS = 0                                                           LN09810
      LINES = 0                                                          LN09820
      I3 = 0                                                             LN09830
C                                                                        LN09840
      RETURN                                                             LN09850
C                                                                        LN09860
  900 FORMAT (2A50)                                                      LN09870
  905 FORMAT (A100)                                                      LN09880
C                                                                        LN09890
      END                                                                LN09900
      SUBROUTINE RDFIL1 (LINBCD,I1,N1,IEOF)                              LN09910
C                                                                        LN09920
      IMPLICIT DOUBLE PRECISION (V)                                     !LN09930
C                                                                        LN09940
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN09950
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN09960
     *               IREC,IRECTL,HID1(2),LSTWD                           LN09970
C                                                                        LN09980
      DOUBLE PRECISION HID,HID1,HMOL                                    &LN09990
      DOUBLE PRECISION STRSV
C                                                                        LN10000
      COMMON /SREJ/ SR(64),SRD(64),TALF(64)                              LN10010
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN10020
      COMMON /HBLOCK/ INBLK1,INBLK2,IO2BND,I86T1,I86T2                   LN10030
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN10040
      COMMON /UNITS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2           LN10050
      COMMON /IC1/ ILIN3                                                 LN10060
      COMMON /MANE/ VNU1(51),STR1(51),ALF1(51),EPP1(51),MOL1(51),        LN10070
     *              HWHM1(51),TMPAL1(51),PSHIF1(51),IFG1(51),            LN10080
     *              MIND1(35),IOUT(51)                                   LN10090
      COMMON /QUANT/ QUANT1(51),QUANTC(51)                               LN10100
      COMMON /LCHAR/ ALIN1(51),ALIN2(40),ALINC(51),ALINE(250)            LN10110
C                                                                        LN10120
      CHARACTER*100 ALIN1,ALIN2,ALINC,ALINE,ALIN(51)                     LN10130
      CHARACTER*50 ZEROFL                                                LN10140
      CHARACTER*27 QUANT1,QUANTC                                         LN10150
      CHARACTER*9 CUP,CLO
      CHARACTER*7 HOL                                                    LN10160
      CHARACTER*1 CFLAG,CBLNK,CMINUS                                     LN10170
      DIMENSION AMOL1(51)                                                LN10180
C                                                                        LN10190
      EQUIVALENCE (MOL1(1),AMOL1(1))                                     LN10200
C                                                                        LN10210
      DATA TEMP0 / 296. /                                                LN10220
      DATA ZEROFL /                                                      LN10230
     *           '00000000000000000000000000000000000000000000000000'/   LN10240
      DATA CBLNK / ' '/,CMINUS / '-'/                                    LN10250
C                                                                        LN10260
      BETA0 = RADCN2/TEMP0                                               LN10270
      IER = 0                                                            LN10280
      IEOF = 0                                                           LN10290
      I1 = 1                                                             LN10300
      N1 = 0                                                             LN10310
      ILIN3 = 0                                                          LN10320
C                                                                        LN10330
   10 CONTINUE                                                           LN10340
      IF (IEOF.EQ.1) GO TO 60                                            LN10350
      IEOF = 1                                                           LN10360
      IDIM = 51                                                          LN10370
      IF (INBLK1.EQ.0) THEN                                              LN10380
         READ (LINBCD,900,END=60) ALIN                                   LN10390
      ELSE                                                               LN10400
         IDIM = 50                                                       LN10410
         DO 20 I = 1, IDIM                                               LN10420
            READ (LINBCD,900,END=30) ALIN(I)                             LN10430
   20    CONTINUE                                                        LN10440
         READ (ALIN(IDIM),905) CFLAG,IFLAG                               LN10450
         IF (IFLAG.LT.0.AND.CFLAG.NE.CBLNK.AND.CFLAG.NE.CMINUS) THEN     LN10460
            IDIM = 51                                                    LN10470
            READ (LINBCD,900,END=30) ALIN(IDIM)                          LN10480
         ENDIF                                                           LN10490
      ENDIF                                                              LN10500
C                                                                        LN10510
      IEOF = 0                                                           LN10520
      GO TO 40                                                           LN10530
   30 IDIM = I-1                                                         LN10540
      IF (IDIM.LE.0) GO TO 60                                            LN10550
   40 IFLGM1 = 0                                                         LN10560
      DO 50 I = 1, 51                                                    LN10570
         IF (I.GT.IDIM) WRITE (ALIN(I),910) ZEROFL,ZEROFL                LN10580
         READ (ALIN(I),915) MOL                                          LN10590
         IF (MOL.EQ.0) GO TO 50                                          LN10600
         M = MOL                                                         LN10610
         IF (MIND1(M).EQ.0) GO TO 50                                     LN10620
         IF (IFLGM1.LE.0) THEN                                           LN10630
            READ (ALIN(I),920) ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,   LN10640
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HOL,IFLGSV   LN10650
            IFLAG = IFLGSV                                               LN10660
            MOL = MOL+100*ISO                                            LN10670
            IF (IFLAG.LT.0) THEN                                         LN10680
               IFLAG = IABS(IFLAG)                                       LN10690
            ELSE                                                         LN10700
               IFLAG = 0                                                 LN10710
            ENDIF                                                        LN10720
            IFLGM1 = IFLAG
            VNUS = VNU                                                   LN10730
C
C           SKIP OVER UNEEDED WAVENUMBERS
C
            IF (VNUS.LT.VMIN) GO TO 50                                   LN12380
            IF (VNUS.GT.VMAX) RETURN                                     LN12390
C
            STR = STRSV/(VNU*(1.0-EXP(-BETA0*VNU)))                      LN10760
            IF (STR.LT.SR(M)) GO TO 50                                   LN10770
            IF (INLTE.EQ.1) CALL VIBQ1 (MOL,IVUP,IVLO)                   LN10780
            ILIN3 = ILIN3+1                                              LN10790
            ILINS = ILIN3                                                LN10795
            WRITE (QUANT1(ILIN3),930) M,ISO,IVUP,IVLO,CUP,CLO            LN10800
C                                                                        LN10810
C           NOTE: SHIFT VARIABLE MAY CONTAIN COUPLING INFORMATION        LN10820
C            - NO PRESSURE SHIFT INFORMATION CURRENTLY PROVIDED,         LN10830
C                  AND COUPLING INFORMATION DELETED -                    LN10840
C                                                                        LN10850
            IF (I86T1.EQ.1) SHIFT = 0.                                   LN10860
C                                                                        LN10870
C           HWHMS = 0. IN HITRAN 86.  CHECK FOR ZERO; IF ZERO            LN10880
C           AND NOT WATER, SET TO HWHMF.  IF WATER, SET TO 5. * HWHMF    LN10890
C                                                                        LN10900
            IF (HWHMS.EQ.0.) THEN                                        LN10910
               HWHMS = HWHMF                                             LN10920
               IF (M.EQ.1) HWHMS = 5.*HWHMF                              LN10930
            ENDIF                                                        LN10940
            WRITE (ALIN1(ILIN3),921) M,ISO,VNU,STRSV,TRANS,HWHMF,        LN10950
     *                              HWHMS,ENERGY,TDEP,SHIFT,IVUP,        LN10960
     *                              IVLO,CUP,CLO,HOL,IFLGSV              LN10970
            VNU1(ILIN3)   = VNU                                          LN10980
            STR1(ILIN3)   = STR                                          LN10990
            ALF1(ILIN3)   = HWHMF                                        LN11000
            EPP1(ILIN3)   = ENERGY                                       LN11010
            MOL1(ILIN3)   = MOL                                          LN11020
            HWHM1(ILIN3)  = HWHMS                                        LN11025
            TMPAL1(ILIN3) = 1.-TDEP                                      LN11030
            PSHIF1(ILIN3) = SHIFT                                        LN11040
            IFG1(ILIN3)   = IFLAG                                        LN11060
            N1 = N1+1                                                    LN11070
C
C           SET FLAG FOR O2 0.0 CM-1 BAND CASE
C
            IF (IO2BND.EQ.1.AND.IFLAG.EQ.3) IO2BND = -1                  LN11080
            IOUT(N1) = ILINS                                             LN11090
         ELSE                                                            LN11100
C
C           LINE COUPLING INFORMATION READ IN
C
            READ (ALIN(I),925) Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG             LN11110
C
C           SET IFLGM1 TO ZERO TO ENSURE THE NEXT PASS USES 
C           REGULAR FORMAT FOR READING LINE INFORMATION
C
            IFLGM1 = 0                                                   LN11120
C
C           SKIP OVER UNEEDED WAVENUMBERS
C
            IF (VNUS.LT.VMIN) GO TO 50
            ILIN3 = ILIN3 + 1                                            LN11130
            ALIN1(ILIN3)  = ALIN(I)                                      LN11140
            VNU1(ILIN3)   = Y1                                           LN11150
            STR1(ILIN3)   = G1                                           LN11160
            ALF1(ILIN3)   = Y2                                           LN11170
            EPP1(ILIN3)   = G2                                           LN11180
            AMOL1(ILIN3)  = Y3                                           LN11190
            HWHM1(ILIN3)  = G3                                           LN11200
            TMPAL1(ILIN3) = Y4                                           LN11210
            PSHIF1(ILIN3) = G4                                           LN11220
            IFG1(ILIN3)   = IFLAG                                        LN11230
         ENDIF                                                           LN11240
C
   50 CONTINUE                                                           LN11250
C                                                                        LN11260
      IF (ILIN3.LT.1.AND.IDIM.LT.50) GO TO 60                            LN11290
      IF (ILIN3.LT.1) GO TO 10                                           LN11300
C                                                                        LN11310
      RETURN                                                             LN11320
C                                                                        LN11330
   60 CONTINUE                                                           LN11340
C                                                                        LN11350
      PRINT 940                                                          LN11360
      IF (VMAX.LT.17000.) THEN                                           LN11370
         PRINT 945,VMAX                                                  LN11380
      ENDIF                                                              LN11390
C                                                                        LN11400
      RETURN                                                             LN11410
C                                                                        LN11420
  900 FORMAT (51(A100))                                                  LN11430
  905 FORMAT (2X,A1,95X,I2)                                              LN11440
  910 FORMAT (2A50)                                                      LN11450
  915 FORMAT (I2)                                                        LN11460
  920 FORMAT (2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,2I3,2A9,
     *        A7,I2)                                                     LN11470
  921 FORMAT (I2,I1,F12.6,1P,D10.3,E10.3,0P,2F5.4,F10.4,F4.2,F8.6,2I3,
     *        2A9,A7,I2)
  925 FORMAT (2X,4(E13.6,E11.4),I2)                                      LN11480
  930 FORMAT (I2,I1,2I3,2A9)                                             LN11490
  940 FORMAT (' TAPE1 IS AT A EOF ',/,                                   LN11510
     *        ' THE 1986 TAPE HAS ONE EOF AT EOT  ')                     LN11520
  945 FORMAT (' THE EOF ON THE 86 TAPE IS AFTER 17000 ',/,               LN11530
     *        ' VMAX = ',F12.5,' YOU MAY HAVE A BAD TAPE ')              LN11540
C                                                                        LN11550
      END                                                                LN11560
      SUBROUTINE RDFIL2 (LINBCD,I2,N2,IEOF)                              LN11570
C                                                                        LN11580
      IMPLICIT DOUBLE PRECISION (V)                                     !LN11590
C                                                                        LN11600
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN11610
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN11620
     *               IREC,IRECTL,HID1(2),LSTWD                           LN11630
C                                                                        LN11640
      DOUBLE PRECISION HID,HID1,HMOL                                    &LN11650
      DOUBLE PRECISION STRSV
C                                                                        LN11660
      COMMON /SREJ/ SR(64),SRD(64),TALF(64)                              LN11670
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN11680
      COMMON /HBLOCK/ INBLK1,INBLK2,IO2BND,I86T1,I86T2                   LN11690
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN11700
      COMMON /UNITS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2           LN11710
      COMMON /IC2/ ILIN3                                                 LN11720
      COMMON /MAINC/ VNUC(51),STRC(51),ALFC(51),EPPC(51),MOLC(51),       LN11730
     *               HWHMC(51),TMPALC(51),PSHIFC(51),IFGC(51),           LN11740
     *               MINDC(35),IOUTC(51)                                 LN11750
      COMMON /QUANT/ QUANT1(51),QUANTC(51)                               LN11760
      COMMON /LCHAR/ ALIN1(51),ALIN2(40),ALINC(51),ALINE(250)            LN11770
C                                                                        LN11780
      CHARACTER*100 ALIN1,ALIN2,ALINC,ALINE,ALIN(51)                     LN11790
      CHARACTER*50 ZEROFL                                                LN11800
      CHARACTER*27 QUANT1,QUANTC                                         LN11810
      CHARACTER*9 CUP,CLO                                                LN11820
      CHARACTER*7 HOL
      CHARACTER*1 CFLAG,CBLNK,CMINUS                                     LN11830
      DIMENSION AMOLC(51)                                                LN11840
C                                                                        LN11850
      EQUIVALENCE (MOLC(1),AMOLC(1))                                     LN11860
C                                                                        LN11870
      DATA TEMP0 / 296. /                                                LN11880
      DATA ZEROFL /                                                      LN11890
     *           '00000000000000000000000000000000000000000000000000'/   LN11900
      DATA CBLNK / ' '/,CMINUS / '-'/                                    LN11910
C                                                                        LN11920
      BETA0 = RADCN2/TEMP0                                               LN11930
      IER = 0                                                            LN11940
      IEOF = 0                                                           LN11950
      I2 = 1                                                             LN11960
      N2 = 0                                                             LN11970
      ILIN3 = 0                                                          LN11980
C                                                                        LN11990
   10 CONTINUE                                                           LN12000
      IF (IEOF.EQ.1) GO TO 60                                            LN12010
      IEOF = 1                                                           LN12020
      IDIM = 51                                                          LN12030
      IF (INBLK2.EQ.0) THEN                                              LN12040
         READ (LINBCD,900,END=60) ALIN                                   LN12050
      ELSE                                                               LN12060
         IDIM = 50                                                       LN12070
         DO 20 I = 1, IDIM                                               LN12080
            READ (LINBCD,900,END=30) ALIN(I)                             LN12090
   20    CONTINUE                                                        LN12100
         READ (ALIN(IDIM),905) CFLAG,IFLAG                               LN12110
         IF (IFLAG.LT.0.AND.CFLAG.NE.CBLNK.AND.CFLAG.NE.CMINUS) THEN     LN12120
            IDIM = 51                                                    LN12130
            READ (LINBCD,900,END=30) ALIN(IDIM)                          LN12140
         ENDIF                                                           LN12150
      ENDIF                                                              LN12160
C                                                                        LN12170
      IEOF = 0                                                           LN12180
      GO TO 40                                                           LN12190
   30 IDIM = I-1                                                         LN12200
      IF (IDIM.LE.0) GO TO 60                                            LN12210
   40 IFLGM1 = 0                                                         LN12220
      DO 50 I = 1, 51                                                    LN12230
         IF (I.GT.IDIM) WRITE (ALIN(I),910) ZEROFL,ZEROFL                LN12240
         READ (ALIN(I),915) MOL                                          LN12250
         IF (MOL.EQ.0) GO TO 50                                          LN12260
         M = MOL                                                         LN12270
         IF (MINDC(M).EQ.0) GO TO 50                                     LN12280
         IF (IFLGM1.LE.0) THEN                                           LN12290
            READ (ALIN(I),920) ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,   LN12300
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HOL,IFLGSV   LN12310
            IFLAG = IFLGSV
            MOL = MOL+100*ISO
            IF (IFLAG.LT.0) THEN                                         LN12320
               IFLAG = IABS(IFLAG)                                       LN12330
            ELSE                                                         LN12340
               IFLAG = 0                                                 LN12350
            ENDIF                                                        LN12360
            IFLGM1 = IFLAG
            VNUS = VNU                                                   LN12370
C
C           SKIP OVER UNEEDED WAVENUMBERS
C
            IF (VNUS.LT.VMIN) GO TO 50                                   LN12380
            IF (VNUS.GT.VMAX) RETURN                                     LN12390
C
            STR = STRSV/(VNU*(1.0-EXP(-BETA0*VNU)))                      LN12400
            IF (STR.LT.SR(M)) GO TO 50                                   LN12410
            IF (INLTE.EQ.1) CALL VIBQ1 (MOL,IVUP,IVLO)                   LN12420
            ILIN3 = ILIN3+1                                              LN12430
            ILINS = ILIN3
            WRITE (QUANTC(ILIN3),930) M,ISO,IVUP,IVLO,CUP,CLO            LN12440
C
C           NOTE: SHIFT VARIABLE MAY CONTAIN COUPLING INFORMATION        LN12450
C            - NO PRESSURE SHIFT INFORMATION CURRENTLY PROVIDED,         LN12460
C                     AND COUPLING INFORMATION DELETED -                 LN12470
C
            IF (I86T2.EQ.1) SHIFT = 0.                                   LN12480
C
C           HWHMS = 0. IN HITRAN 86.  CHECK FOR ZERO; IF ZERO
C           AND NOT WATER, SET TO HWHMF.  IF WATER, SET TO 5. * HWHMF
C
            IF (HWHMS.EQ.0.) THEN                                        LN12490
               HWHMS = HWHMF                                             LN12500
               IF (M.EQ.1) HWHMS = 5.*HWHMF                              LN12510
            ENDIF                                                        LN12520
            WRITE (ALINC(ILIN3),921) M,ISO,VNU,STRSV,TRANS,HWHMF,        LN12530
     *                               HWHMS,ENERGY,TDEP,SHIFT,IVUP,       LN12540
     *                               IVLO,CUP,CLO,HOL,IFLGSV             LN12550
            VNUC(ILIN3)   = VNU                                          LN12560
            STRC(ILIN3)   = STR                                          LN12570
            ALFC(ILIN3)   = HWHMF                                        LN12580
            EPPC(ILIN3)   = ENERGY                                       LN12590
            MOLC(ILIN3)   = MOL                                          LN12600
            HWHMC(ILIN3)  = HWHMS                                        LN12610
            TMPALC(ILIN3) = 1.-TDEP                                      LN12620
            PSHIFC(ILIN3) = SHIFT                                        LN12630
            IFGC(ILIN3)   = IFLAG                                        LN12640
            N2 = N2+1                                                    LN12650
C                                                                        LN12660
C           SET FLAG FOR O2 0.0 CM-1 BAND CASE                           LN12670
C                                                                        LN12680
            IF (IO2BND.EQ.1.AND.IFLAG.EQ.3) IO2BND = -1                  LN12690
            IOUTC(N2) = ILINS                                            LN12700
         ELSE                                                            LN12710
C                                                                        LN12720
C           LINE COUPLING INFORMATION READ IN                            LN12730
C                                                                        LN12740
            READ (ALIN(I),925) Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG             LN12750
C
C           SET IFLGM1 TO ZERO TO ENSURE THE NEXT PASS USES 
C           REGULAR FORMAT FOR READING LINE INFORMATION
C
            IFLGM1 = 0                                                   LN12760
C
C           SKIP OVER UNEEDED WAVENUMBERS
C
            IF (VNUS.LT.VMIN) GO TO 50
            ILIN3 = ILIN3 + 1                                            LN12770
            ALINC(ILIN3)  = ALIN(I)                                      LN12780
            VNUC(ILIN3)   = Y1                                           LN12790
            STRC(ILIN3)   = G1                                           LN12800
            ALFC(ILIN3)   = Y2                                           LN12810
            EPPC(ILIN3)   = G2                                           LN12820
            AMOLC(ILIN3)  = Y3                                           LN12830
            HWHMC(ILIN3)  = G3                                           LN12835
            TMPALC(ILIN3) = Y4                                           LN12840
            PSHIFC(ILIN3) = G4                                           LN12850
            IFGC(ILIN3)   = IFLAG                                        LN12870
         ENDIF                                                           LN12890
C                                                                        LN12900
   50 CONTINUE                                                           LN12910
C                                                                        LN12920
      IF (ILIN3.LT.1.AND.IDIM.LT.50) GO TO 60                            LN12950
      IF (ILIN3.LT.1) GO TO 10                                           LN12960
C                                                                        LN12970
      RETURN                                                             LN12980
C                                                                        LN12990
   60 CONTINUE                                                           LN13000
      PRINT 940                                                          LN13010
C                                                                        LN13020
      RETURN                                                             LN13030
C                                                                        LN13040
  900 FORMAT (51(A100))                                                  LN13050
  905 FORMAT (2X,A1,95X,I2)                                              LN13060
  910 FORMAT (2A50)                                                      LN13070
  915 FORMAT (I2)                                                        LN13080
  920 FORMAT (2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,2I3,2A9,
     *        A7,I2)                                                     LN13090
  921 FORMAT (I2,I1,F12.6,1P,D10.3,E10.3,0P,2F5.4,F10.4,F4.2,F8.6,2I3,
     *        2A9,A7,I2)
  925 FORMAT (2X,4(E13.6,E11.4),I2)                                      LN13100
  930 FORMAT (I2,I1,2I3,2A9)                                             LN13110
  940 FORMAT (' TAPE2 IS AT A EOF ')                                     LN13130
C                                                                        LN13140
      END                                                                LN13150
      SUBROUTINE VIBQ1 (MOL,IVUP,IVLO)                                   LN13160
C                                                                        LN13170
      IMPLICIT DOUBLE PRECISION (V)                                     !LN13180
C                                                                        LN13190
C                                                                        LN13200
C     THIS SUBROUTINE MAPS THE TWO VIBRATIONAL LEVEL                     LN13210
C     ID'S OF SELECTED MOLECULES AGAINST STORED VIB STATE                LN13220
C     ID'S.  MOL IS ADJUSTED TO INCLUDE ADDITIONAL IDENTIFIERS.          LN13230
C          IJ ----- UPPER STATE ID                                       LN13240
C          KL ----- LOWER STATE ID                                       LN13250
C          MN ----- ORDINARY MOLECULAR ID                                LN13260
C                                                                        LN13270
      DIMENSION MH2O(8),MCO2(26),MO3(18),MCO(3),MNO(3),MNOA(3)           LN13280
C                                                                        LN13290
      DATA MH2O / 1,2,3,4,5,6,7,8 /                                      LN13300
      DATA MCO2 / 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,23,24,25,36,    LN13310
     *            37,38,43,47,48,50 /                                    LN13315
      DATA MO3 / 1,2,5,4,3,8,7,14,13,12,18,27,43,100,101,102,103,104 /   LN13320
      DATA MCO / 1,2,3 /                                                 LN13330
      DATA MNO / 1,2,3 /,MNOA / 11,12,13 /                               LN13340
                                                                         LN13350
C                                                                        LN13360
      DATA NUMH2O,NUMCO2,NUMO3,NUMCO,NUMNO / 8,26,18,3,3 /               LN13370
      MOL2 = MOL                                                         LN13380
      MOL1 = MOD(MOL2,100)                                               LN13400
      IF (MOL1.GT.8) GO TO 210                                           LN13390
C                                                                        LN13420
C     MOL1 IS THE ORIGINAL MOLECULAR ID                                  LN13430
C     MOL2 IS THE NEW TAGGED MOL ID                                      LN13440
C                                                                        LN13460
      IF (MOL1.NE.1) GO TO 10                                            LN13470
C                                                                        LN13480
C     WATER VAPOR LINE                                                   LN13490
C                                                                        LN13500
C     DATA MH2O / 1,2,3,4,5,6,7,8/                                       LN13510
C                                                                        LN13520
      NUPP = IVUP                                                        LN13530
      NLOW = IVLO                                                        LN13540
      IF (NLOW.GT.NUMH2O) RETURN                                         LN13550
      IF (NUPP.GT.NUMH2O) RETURN                                         LN13560
      MOL = MOL2+1000*NLOW+100000*NUPP                                   LN13570
C                                                                        LN13580
      RETURN                                                             LN13590
C                                                                        LN13600
C     CO2 LINE                                                           LN13610
C                                                                        LN13620
   10 IF (MOL1.NE.2) GO TO 60                                            LN13630
C                                                                        LN13640
C     DATA NUMH2O,NUMCO2,NUMO3,NUMCO,NUMNO/ 8,26,18,3,3 /                LN13650
C                                                                        LN13660
      DO 20 I = 1, NUMCO2                                                LN13670
         IMAP = I                                                        LN13680
         IF (IVUP.EQ.MCO2(I)) GO TO 30                                   LN13690
   20 CONTINUE                                                           LN13700
C                                                                        LN13710
      RETURN                                                             LN13720
C                                                                        LN13730
   30 NUPP = IMAP                                                        LN13740
      DO 40 I = 1, NUMCO2                                                LN13750
         IMAP = I                                                        LN13760
         IF (IVLO.EQ.MCO2(I)) GO TO 50                                   LN13770
   40 CONTINUE                                                           LN13780
C                                                                        LN13790
      RETURN                                                             LN13800
C                                                                        LN13810
   50 NLOW = IMAP                                                        LN13820
      MOL = MOL2+1000*NLOW+100000*NUPP                                   LN13830
C                                                                        LN13840
      RETURN                                                             LN13850
C                                                                        LN13860
   60 CONTINUE                                                           LN13870
C                                                                        LN13880
      IF (MOL1.NE.3) GO TO 110                                           LN13890
C                                                                        LN13900
C     OZONE LINE                                                         LN13910
C                                                                        LN13920
C     DATA MO3 / 1,2,5,4,3,8,7,14,13,12,18,27,43,100,101,102,103,104 /   LN13930
C     DATA NUMH2O,NUMCO2,NUMO3,NUMCO,NUMNO/ 8,26,18,3,3 /                LN13940
C                                                                        LN13950
      DO 70 I = 1, NUMO3                                                 LN13960
         IMAP = I                                                        LN13970
         IF (IVUP.EQ.MO3(I)) GO TO 80                                    LN13980
   70 CONTINUE                                                           LN13990
C                                                                        LN14000
      RETURN                                                             LN14010
C                                                                        LN14020
   80 NUPP = IMAP                                                        LN14030
      DO 90 I = 1, NUMO3                                                 LN14040
         IMAP = I                                                        LN14050
         IF (IVLO.EQ.MO3(I)) GO TO 100                                   LN14060
   90 CONTINUE                                                           LN14070
C                                                                        LN14080
      RETURN                                                             LN14090
C                                                                        LN14100
  100 NLOW = IMAP                                                        LN14110
      MOL = MOL2+1000*NLOW+100000*NUPP                                   LN14120
C                                                                        LN14130
      RETURN                                                             LN14140
C                                                                        LN14150
  110 CONTINUE                                                           LN14160
      IF (MOL1.NE.5) GO TO 160                                           LN14170
C                                                                        LN14180
C     CARBON MONOXIDE LINE                                               LN14190
C                                                                        LN14200
C     DATA MCO / 1,2,3 /                                                 LN14210
C     DATA NUMH2O,NUMCO2,NUMO3,NUMCO,NUMNO/ 8,26,18,3,3 /                LN14220
C                                                                        LN14230
      DO 120 I = 1, NUMCO                                                LN14240
         IMAP = I                                                        LN14250
         IF (IVUP.EQ.MCO(I)) GO TO 130                                   LN14260
  120 CONTINUE                                                           LN14270
C                                                                        LN14280
      RETURN                                                             LN14290
C                                                                        LN14300
  130 NUPP = IMAP                                                        LN14310
      DO 140 I = 1, NUMCO                                                LN14320
         IMAP = I                                                        LN14330
         IF (IVLO.EQ.MCO(I)) GO TO 150                                   LN14340
  140 CONTINUE                                                           LN14350
C                                                                        LN14360
      RETURN                                                             LN14370
C                                                                        LN14380
  150 NLOW = IMAP                                                        LN14390
      MOL = MOL2+1000*NLOW+100000*NUPP                                   LN14400
C                                                                        LN14410
      RETURN                                                             LN14420
C                                                                        LN14430
  160 CONTINUE                                                           LN14440
      IF (MOL1.NE.8) GO TO 210                                           LN14450
C                                                                        LN14460
C     NITRIC OXIDE LINE                                                  LN14470
C                                                                        LN14480
C     DATA NUMH2O,NUMCO2,NUMO3,NUMCO,NUMNO/ 8,26,18,3,3 /                LN14490
C     DATA MNO / 1,2,3 /,MNOA /11,12,13/                                 LN14500
C                                                                        LN14510
      DO 170 I = 1, NUMNO                                                LN14520
         IMAP = I                                                        LN14530
         IF (IVUP.EQ.MNO(I)) GO TO 180                                   LN14540
         IF (IVUP.EQ.MNOA(I)) GO TO 180                                  LN14550
  170 CONTINUE                                                           LN14560
C                                                                        LN14570
      RETURN                                                             LN14580
C                                                                        LN14590
  180 NUPP = IMAP                                                        LN14600
      DO 190 I = 1, NUMNO                                                LN14610
         IMAP = I                                                        LN14620
         IF (IVLO.EQ.MNO(I)) GO TO 200                                   LN14630
         IF (IVLO.EQ.MNOA(I)) GO TO 200                                  LN14640
  190 CONTINUE                                                           LN14650
C                                                                        LN14660
      RETURN                                                             LN14670
C                                                                        LN14680
  200 NLOW = IMAP                                                        LN14690
      MOL = MOL2+1000*NLOW+100000*NUPP                                   LN14700
C                                                                        LN14710
  210 RETURN                                                             LN14720
C                                                                        LN14730
      END                                                                LN14740
      SUBROUTINE RDFL82 (LINBCD,ILO,NMAX,IEOF,VLST)                      LN14750
C                                                                        LN14760
      IMPLICIT DOUBLE PRECISION (V)                                     !LN14770
C                                                                        LN14780
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN14790
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN14800
     *               IREC,IRECTL,HID1(2),LSTWD                           LN14810
C                                                                        LN14820
      DOUBLE PRECISION HID,HID1,HMOL                                    &LN14830
C                                                                        LN14840
      COMMON /SREJ/ SR(64),SRD(64),TALF(64)                              LN14850
      COMMON /HOL/ HOL82(40)                                             LN14860
      CHARACTER HOL82*35                                                 LN14870
      COMMON /TRAC/ VNU(40),STR(40),ALF(40),EPP(40),MOL(40),HWHMF(40),   LN14880
     *              TMPALF(40),PSHIFT(40),IFLG(40),MIND2(35)             LN14890
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN14900
      COMMON /HBLOCK/ INBLK1,INBLK2,IO2BND,I86T1,I86T2                   LN14910
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN14920
      COMMON /UNITS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2           LN14930
      COMMON /LCHAR/ ALIN1(51),ALIN(40),ALINC(51),ALINE(250)             LN14940
      DIMENSION IDATE(40),ISO(40)                                        LN14950
C                                                                        LN14960
      CHARACTER*100 ALIN1,ALIN,ALINC,ALINE                               LN14970
      CHARACTER*50 ZEROFL                                                LN14980
C                                                                        LN14990
      DATA ZEROFL /                                                      LN15000
     *           '00000000000000000000000000000000000000000000000000'/   LN15010
      DATA TEMP0 / 296. /                                                LN15020
C                                                                        LN15030
C     HALF WIDTH TEMPERATURE DEPENDANCE (TALF(M))                        LN15040
C     STORED IN DATA STATEMENTS FROM MOLEC FOR DENSITY FORMALISM         LN15050
C                                                                        LN15060
      BETA0 = RADCN2/TEMP0                                               LN15070
      ILO = 0                                                            LN15080
      NMAX = 0                                                           LN15090
      IER = 0                                                            LN15100
      IEOF = 0                                                           LN15110
C                                                                        LN15120
      NLIN = 1                                                           LN15130
      KEOF = 0                                                           LN15140
C                                                                        LN15150
   10 CONTINUE                                                           LN15160
      IF (KEOF.EQ.1) GO TO 80                                            LN15170
      IF (INBLK2.EQ.0) THEN                                              LN15180
         READ (LINBCD,900,END=80) NLIN,                                  LN15190
     *                           (VNU(K),STR(K),ALF(K),EPP(K),HOL82(K),  LN15200
     *                            IDATE(K),ISO(K),MOL(K),K=1,NLIN)       LN15210
      ELSE                                                               LN15220
         NLIN = 40                                                       LN15230
         KEOF = 1                                                        LN15240
         DO 20 I = 1, NLIN                                               LN15250
            READ (LINBCD,905,END=30) VNU(I),STR(I),ALF(I),EPP(I),        LN15260
     *                               HOL82(I),IDATE(I),ISO(I),MOL(I)     LN15270
   20    CONTINUE                                                        LN15280
         KEOF = 0                                                        LN15290
      ENDIF                                                              LN15300
      IEOF = 0                                                           LN15310
      GO TO 40                                                           LN15320
   30 NLIN = I-1                                                         LN15330
      IF (NLIN.LE.0) GO TO 80                                            LN15340
C                                                                        LN15350
   40 INUM = 0                                                           LN15360
      IF (VNU(NLIN).LT.VMIN) GO TO 10                                    LN15370
      DO 50 I = 1, NLIN                                                  LN15380
         WRITE (ALIN(I),910) VNU(I),STR(I),ALF(I),EPP(I),HOL82(I),       LN15390
     *                       IDATE(I),ISO(I),MOL(I)                      LN15400
         IF (VNU(I).LT.VLST) THEN                                        LN15410
            IF (LINBCD.EQ.1) IER = 1                                     LN15420
            IF (LINBCD.EQ.2) IER = 2                                     LN15430
            NLIN = I-1                                                   LN15440
            WRITE (IPR,915) VNU(I),VLST,IER                              LN15450
            IEOF = 999                                                   LN15460
C                                                                        LN15470
            RETURN                                                       LN15480
C                                                                        LN15490
         ENDIF                                                           LN15500
C                                                                        LN15510
         VLST = VNU(I)                                                   LN15520
         IF (VNU(I).GT.VMAX) GO TO 70                                    LN15530
         IF (VNU(I).LT.VMIN) GO TO 50                                    LN15540
         M = MOL(I)                                                      LN15550
         IF (MIND2(M).NE.1) MOL(I) = 0                                   LN15560
         IF (MOL(I).EQ.0) GO TO 50                                       LN15570
         STR(I) = STR(I)/(VNU(I)*(1.0-EXP(-BETA0*VNU(I))))               LN15580
         HWHMF(I) = ALF(I)                                               LN15590
         IF (M.EQ.1) HWHMF(I) = ALF(I)*5.                                LN15600
         TMPALF(I) = TALF(M)                                             LN15610
         PSHIFT(I) = 0.                                                  LN15620
         IFLG(I) = 0                                                     LN15630
         IF (STR(I).LT.SR(M)) MOL(I) = 0                                 LN15640
         IF (MOL(I).EQ.0) GO TO 50                                       LN15650
         NMAX = I                                                        LN15660
         INUM = INUM+1                                                   LN15670
         IF (INUM.EQ.1) ILO = I                                          LN15680
   50 CONTINUE                                                           LN15690
C                                                                        LN15700
      IF (NLIN.LT.40) THEN                                               LN15710
         DO 60 I = NLIN+1, 40                                            LN15720
            WRITE (ALIN(I),920) ZEROFL,ZEROFL                            LN15730
   60    CONTINUE                                                        LN15740
      ENDIF                                                              LN15750
C                                                                        LN15760
      IF (INUM.LE.0) GO TO 10                                            LN15770
      IF (INLTE.EQ.1)                                                    LN15780
     *     CALL VIBQU (ILO,NMAX,NLIN,VNU,STR,ALF,EPP,MOL,ISO)            LN15790
      IF (KEOF.EQ.1) IEOF = 1                                            LN15800
C                                                                        LN15810
      RETURN                                                             LN15820
C                                                                        LN15830
   70 IF (NMAX.LT.1) IEOF = 99                                           LN15840
C                                                                        LN15850
      RETURN                                                             LN15860
C                                                                        LN15870
   80 CONTINUE                                                           LN15880
      CLOSE (LINBCD)                                                     LN15890
      IF (LINBCD.EQ.1)                                                   LN15900
     *     OPEN (1,FILE='TAPE1',STATUS='OLD',FORM='FORMATTED')           LN15910
      IF (LINBCD.EQ.2)                                                   LN15920
     *     OPEN (2,FILE='TAPE2',STATUS='OLD',FORM='FORMATTED')           LN15930
      IEOF = IEOF+1                                                      LN15940
C                                                                        LN15950
      LSTFIL = VMAX/100.+1.                                              LN15960
C                                                                        LN15970
      IF (IEOF.GE.LSTFIL) RETURN                                         LN15980
C                                                                        LN15990
      GO TO 10                                                           LN16000
C                                                                        LN16010
  900 FORMAT (I10,40(F10.3,E10.3,F5.3,F10.3,A35,I3,I4,I3))               LN16020
  905 FORMAT (F10.3,E10.3,F5.3,F10.3,A35,I3,I4,I3)                       LN16030
  910 FORMAT (F10.3,1P,E10.3,0P,F5.3,F10.3,A35,I3,I4,I3)                 LN16040
  915 FORMAT ('  LINE OUT OF ORDER  BAD LINE =',F10.3,/,                 LN16050
     *        ' LAST GOOD LINE =',F10.3,' ON UNIT',I3,///)               LN16060
  920 FORMAT (2A50)                                                      LN16070
C                                                                        LN16080
      END                                                                LN16090
      SUBROUTINE VIBQU (ILO,NMAX,NLIN,VNU,STR,ALF,EPP,MOL,NSO82)         LN16100
C                                                                        LN16110
      IMPLICIT DOUBLE PRECISION (V)                                     !LN16120
C                                                                        LN16130
C                                                                        LN16140
C    THIS SUBROUTINE COMPARES THE TWO VIBRATIONAL LEVEL                  LN16150
C    ID'S OF SELECTED MOLECULES AGAINST STORED VIB STATE                 LN16160
C    ID'S.  MOL IS ADJUSTED TO INCLUDE ADDITIONAL IDENTIFIERS.           LN16170
C          IJ ----- UPPER STATE ID                                       LN16180
C          KL ----- LOWER STATE ID                                       LN16190
C          MN ----- ORDINARY MOLECULAR ID                                LN16200
C                                                                        LN16210
C                                                                        LN16220
      DIMENSION VNU(*),STR(*),ALF(*),EPP(*),MOL(*),NSO82(*)              LN16230
      COMMON /HOL/ HOL82(40)                                             LN16240
      CHARACTER*35 HOL82,TI                                              LN16250
      CHARACTER*10 AQH2O(8),AQCO2(26),AQO3(18),AQCO(3),AQNO(3),          LN16260
     *             BLNK,HTEST,QIB1,QIB2                                  LN16270
C                                                                        LN16280
      DATA NUMH2O,NUMCO2,NUMO3,NUMCO,NUMNO/ 8,26,18,3,3 /                LN16290
C                                                                        LN16300
      DATA BLNK /'          '/                                           LN16310
C                                                                        LN16320
      DATA AQH2O/                                                        LN16330
     *       '000' ,                                                     LN16340
     *       '010' ,                                                     LN16350
     *       '020' ,                                                     LN16360
     *       '100' ,                                                     LN16370
     *       '001' ,                                                     LN16380
     *       '030' ,                                                     LN16390
     *       '110' ,                                                     LN16400
     *       '011' /                                                     LN16410
C                                                                        LN16420
      DATA AQCO2/                                                        LN16430
     *      '00001' , '01101' , '10002' , '02201' , '10001' , '11102' ,  LN16440
     *      '03301' , '11101' , '00011' , '20003' , '12202' , '20002' ,  LN16450
     *      '04401' , '12201' , '20001' , '01111' , '10012' , '02211' ,  LN16460
     *      '10011' , '11112' , '03311' , '11111' , '20013' , '04411' ,  LN16470
     *      '20012' , '20011' /                                          LN16490
C                                                                        LN16600
      DATA AQO3/                                                         LN16610
     *       '000' ,                                                     LN16620
     *       '010' ,                                                     LN16630
     *       '001' ,                                                     LN16640
     *       '100' ,                                                     LN16650
     *       '020' ,                                                     LN16660
     *       '011' ,                                                     LN16670
     *       '110' ,                                                     LN16680
     *       '002' ,                                                     LN16690
     *       '101' ,                                                     LN16700
     *       '200' ,                                                     LN16710
     *       '111' ,                                                     LN16720
     *       '003' ,                                                     LN16730
     *       '004' ,                                                     LN16740
     *       '005' ,                                                     LN16750
     *       '006' ,                                                     LN16760
     *       '007' ,                                                     LN16770
     *       '008' ,                                                     LN16780
     *       '009' /                                                     LN16790
C                                                                        LN16800
      DATA AQCO/                                                         LN16810
     *       '0' ,                                                       LN16820
     *       '1' ,                                                       LN16830
     *       '2' /                                                       LN16840
C                                                                        LN16850
      DATA AQNO/                                                         LN16860
     *       '0' ,                                                       LN16870
     *       '1' ,                                                       LN16880
     *       '2' /                                                       LN16890
C                                                                        LN16900
      IF (ILO.LT.1) RETURN                                               LN16910
C                                                                        LN16920
      DO 260 I = ILO, NMAX                                               LN16930
         MOL2 = MOL(I)                                                   LN16940
         CALL ISTOPE (NSO82(I),ISO85,MOL2,1)                             LN16950
         MOL2 = MOL2+100*ISO85                                           LN16960
         IF (MOL(I).EQ.0) GO TO 260                                      LN16970
         QIB1 = BLNK                                                     LN16980
         QIB2 = BLNK                                                     LN16990
         TI = HOL82(I)                                                   LN17000
         MOL1 = MOD(MOL2,100)                                            LN17010
         IF (MOL1.GT.8) GO TO 250                                        LN17020
C                                                                        LN17030
C     MOL1 IS THE ORIGINAL MOLECULAR ID                                  LN17040
C     MOL2 IS THE NEW TAGGED MOL ID                                      LN17050
C                                                                        LN17060
         IF (MOL1.NE.1) GO TO 50                                         LN17070
C                                                                        LN17080
C     WATER VAPOR LINE                                                   LN17090
C                                                                        LN17100
         QIB1(1:1) = TI(23:23)                                           LN17110
         QIB1(2:2) = TI(25:25)                                           LN17120
         QIB1(3:3) = TI(27:27)                                           LN17130
         QIB2(1:1) = TI(30:30)                                           LN17140
         QIB2(2:2) = TI(32:32)                                           LN17150
         QIB2(3:3) = TI(34:34)                                           LN17160
         NUPP = 0                                                        LN17170
         DO 10 NAQ = 1, NUMH2O                                           LN17180
            HTEST = AQH2O(NAQ)                                           LN17190
            IF (QIB1.NE.HTEST) GO TO 10                                  LN17200
            NUPP = NAQ                                                   LN17210
            GO TO 20                                                     LN17220
   10    CONTINUE                                                        LN17230
         GO TO 250                                                       LN17240
   20    NLOW = 0                                                        LN17250
         DO 30 NAQ = 1, NUMH2O                                           LN17260
            HTEST = AQH2O(NAQ)                                           LN17270
            IF (QIB2.NE.HTEST) GO TO 30                                  LN17280
            NLOW = NAQ                                                   LN17290
            GO TO 40                                                     LN17300
   30    CONTINUE                                                        LN17310
         GO TO 250                                                       LN17320
   40    MOL2 = MOL2+1000*NLOW+100000*NUPP                               LN17330
C                                                                        LN17340
CPRT  PRINT 900,MOL2,TI                                                  LN17350
C                                                                        LN17360
         GO TO 250                                                       LN17370
   50    CONTINUE                                                        LN17380
         IF (MOL1.NE.2) GO TO 100                                        LN17390
C                                                                        LN17400
C     CO2 LINE                                                           LN17410
C                                                                        LN17420
         QIB1(1:1) = TI(4:4)                                             LN17430
         QIB1(2:2) = TI(6:6)                                             LN17440
         QIB1(3:3) = TI(8:8)                                             LN17450
         QIB1(4:4) = TI(10:10)                                           LN17460
         QIB1(5:5) = TI(12:12)                                           LN17470
         QIB2(1:1) = TI(19:19)                                           LN17480
         QIB2(2:2) = TI(21:21)                                           LN17490
         QIB2(3:3) = TI(23:23)                                           LN17500
         QIB2(4:4) = TI(25:25)                                           LN17510
         QIB2(5:5) = TI(27:27)                                           LN17520
         NUPP = 0                                                        LN17530
         DO 60 NAQ = 1, NUMCO2                                           LN17540
            HTEST = AQCO2(NAQ)                                           LN17550
            IF (QIB1.NE.HTEST) GO TO 60                                  LN17560
            NUPP = NAQ                                                   LN17570
            GO TO 70                                                     LN17580
   60    CONTINUE                                                        LN17590
         GO TO 250                                                       LN17600
   70    NLOW = 0                                                        LN17610
         DO 80 NAQ = 1, NUMCO2                                           LN17620
            HTEST = AQCO2(NAQ)                                           LN17630
            IF (QIB2.NE.HTEST) GO TO 80                                  LN17640
            NLOW = NAQ                                                   LN17650
            GO TO 90                                                     LN17660
   80    CONTINUE                                                        LN17670
         GO TO 250                                                       LN17680
   90    MOL2 = MOL2+1000*NLOW+100000*NUPP                               LN17690
C                                                                        LN17700
CPRT  PRINT 900,MOL2,TI                                                  LN17710
C                                                                        LN17720
         GO TO 250                                                       LN17730
  100    CONTINUE                                                        LN17740
C                                                                        LN17750
         IF (MOL1.NE.3) GO TO 150                                        LN17760
C                                                                        LN17770
C     OZONE LINE                                                         LN17780
C                                                                        LN17790
         QIB1(1:3) = TI( 1: 6)                                           LN17800
         QIB2(1:3) = TI(14:16)                                           LN17830
         NUPP = 0                                                        LN17860
         DO 110 NAQ = 1, NUMO3                                           LN17870
            HTEST = AQO3(NAQ)                                            LN17880
            IF (QIB1.NE.HTEST) GO TO 110                                 LN17890
            NUPP = NAQ                                                   LN17900
            GO TO 120                                                    LN17910
  110    CONTINUE                                                        LN17920
         GO TO 250                                                       LN17930
  120    NLOW = 0                                                        LN17940
         DO 130 NAQ = 1, NUMO3                                           LN17950
            HTEST = AQO3(NAQ)                                            LN17960
            IF (QIB2.NE.HTEST) GO TO 130                                 LN17970
            NLOW = NAQ                                                   LN17980
            GO TO 140                                                    LN17990
  130    CONTINUE                                                        LN18000
         GO TO 250                                                       LN18010
  140    MOL2 = MOL2+1000*NLOW+100000*NUPP                               LN18020
         GO TO 250                                                       LN18030
C                                                                        LN18040
  150    CONTINUE                                                        LN18050
         IF (MOL1.NE.5) GO TO 200                                        LN18060
C                                                                        LN18070
C     CARBON MONOXIDE LINE                                               LN18080
C                                                                        LN18090
         QIB1 = TI(8:8)                                                  LN18100
         QIB2 = TI(16:16)                                                LN18110
         NUPP = 0                                                        LN18120
         DO 160 NAQ = 1, NUMCO                                           LN18130
            HTEST = AQCO(NAQ)                                            LN18140
            IF (QIB1.NE.HTEST) GO TO 160                                 LN18150
            NUPP = NAQ                                                   LN18160
            GO TO 170                                                    LN18170
  160    CONTINUE                                                        LN18180
         GO TO 250                                                       LN18190
  170    NLOW = 0                                                        LN18200
         DO 180 NAQ = 1, NUMCO                                           LN18210
            HTEST = AQCO(NAQ)                                            LN18220
            IF (QIB2.NE.HTEST) GO TO 180                                 LN18230
            NLOW = NAQ                                                   LN18240
            GO TO 190                                                    LN18250
  180    CONTINUE                                                        LN18260
         GO TO 250                                                       LN18270
  190    MOL2 = MOL2+1000*NLOW+100000*NUPP                               LN18280
C                                                                        LN18290
CPRT  PRINT 900,MOL2,TI                                                  LN18300
C                                                                        LN18310
         GO TO 250                                                       LN18320
C                                                                        LN18330
  200    CONTINUE                                                        LN18340
         IF (MOL1.NE.8) GO TO 250                                        LN18350
C                                                                        LN18360
C     NITRIC OXIDE LINE                                                  LN18370
C                                                                        LN18380
         QIB1 = TI(10:10)                                                LN18390
         QIB2 = TI(20:20)                                                LN18400
         NUPP = 0                                                        LN18410
         DO 210 NAQ = 1, NUMNO                                           LN18420
            HTEST = AQNO(NAQ)                                            LN18430
            IF (QIB1.NE.HTEST) GO TO 210                                 LN18440
            NUPP = NAQ                                                   LN18450
            GO TO 220                                                    LN18460
  210    CONTINUE                                                        LN18470
         GO TO 250                                                       LN18480
  220    NLOW = 0                                                        LN18490
         DO 230 NAQ = 1, NUMNO                                           LN18500
            HTEST = AQNO(NAQ)                                            LN18510
            IF (QIB2.NE.HTEST) GO TO 230                                 LN18520
            NLOW = NAQ                                                   LN18530
            GO TO 240                                                    LN18540
  230    CONTINUE                                                        LN18550
         GO TO 250                                                       LN18560
  240    MOL2 = MOL2+1000*NLOW+100000*NUPP                               LN18570
  250    CONTINUE                                                        LN18580
C                                                                        LN18590
  260    MOL(I) = MOL2                                                   LN18600
C                                                                        LN18610
      RETURN                                                             LN18620
C                                                                        LN18630
C  900 FORMAT (' MOL2 = ',I10,' TI = ',A35)                              LN18640
C                                                                        LN18650
      END                                                                LN18660
      SUBROUTINE CPSTOR                                                  LN18670
C                                                                        LN18680
C     THIS SUBROUTINE WRITES OUT THE LINES AND THEIR COUPLING            LN18690
C     COEFFICIENTS TO TAPE2 FOR USE BY LNFL                              LN18700
C                                                                        LN18710
      COMMON /CLINES/ CPLINS(598)                                        LN18720
      CHARACTER CPLINS*100, HQ*7                                         LN18730
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN18740
      COMMON /CPLMOL/ MOLCPL(35),NCPL                                    LN18750
      CHARACTER CUP*9, CLO*9
C                                                                        LN18760
      DO 10 I = 1,82,2                                                   LN18770
         READ (CPLINS(I),920) ISO,VNU,STR,TRANS,HWHM,HWHMS,ENERGY,
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HQ,IFLAG 
         WRITE (2,920) ISO,VNU,STR,TRANS,HWHM,HWHMS,ENERGY,
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HQ,IFLAG 
C 
        READ (CPLINS(I+1),925) ISO,Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG
C**********************
C     CORRECTIONS TO THE LINE COUPLING FOR OXYGEN  (21 AUG 92; SAC)
C
C     Y0 IS TO CORRECT FOR THE NEGLECTED CONTRIBUTION OF THE NONRESONANT LINES
C
C     FACTOR IS TO BRING THE RESULTS IN CONFORMITY WITH THE WESTWATER DATA
C     AND PROVIDES ESSENTIAL AGREEMENT WITH ROSENKRANZ FOR THE Y'S
C
C**********************
C       The value of Y0RES has been changed from 0.017 to 0.013
C       based on the ARM APRIL, 1994 IOP    (05 Feb. 96)
C
        DATA Y0RES/0.013/, FO2/1.22/
C**********************
        IF ((VNU .GT. 0.000011) .AND. (IFLAG.EQ.-1)) THEN
           YY = Y0RES
           IF (VNU .GT. 3.9) YY = Y0RES/2
           Y1  = FO2 * Y1  - YY
           Y2  = FO2 * Y2  - YY
           Y3  = FO2 * Y3  - YY
           Y4  = FO2 * Y4  - YY
        ENDIF
C*********************
C
C
        WRITE (2,925)          ISO,Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG
C
   10 CONTINUE                                                           LN18790
C                                                                        LN18760
C   REMEMBER TO CHECK AND FIX SHIFT = 0. PROBLEM
C                                                                        LN11550
      DO 40 I = 83,NCPL                                                  LN18770
         WRITE (2,900) CPLINS(I)                                         LN18780
   40 CONTINUE                                                           LN18790
C                                                                        LN18800
      REWIND 2                                                           LN18810
C                                                                        LN18820
      RETURN                                                             LN18830
C                                                                        LN18840
  900 FORMAT (A100)                                                      LN18850
  920 FORMAT (I3,F12.6,1P,2E10.3,0P,2F5.4,F10.4,F4.2,F8.6,2I3,2A9,A7,I2) LN11470
  925 FORMAT (I2,1P,4(E13.6,E11.4),0P,I2)                                LN11480
C                                                                        LN18860
      END                                                                LN18870
      BLOCK DATA                                                         LN18880
C                                                                        LN18890
      IMPLICIT DOUBLE PRECISION (V)                                     !LN18900
C                                                                        LN18910
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN18920
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN18930
C                                                                        LN18940
      DOUBLE PRECISION HID,HTIME,HDATE,HID1,HMOL                        &LN18950
C                                                                        LN18960
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN18970
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN18980
     *               IREC,IRECTL,HID1(2),LSTWD                           LN18990
      COMMON /BUFIDC/ CMOL(64),CHID10,CHID08                             LN19000
      CHARACTER CMOL*6,CHID10*8,CHID08*8                                 LN19010
      COMMON /MANE/ VNU1(51),STR1(51),ALF1(51),EPP1(51),MOL1(51),        LN19020
     *              HWHM1(51),TMPAL1(51),PSHIF1(51),IFG1(51),            LN19030
     *              MIND1(35),IOUT(51)                                   LN19040
      COMMON /MAINC/ VNUC(51),STRC(51),ALFC(51),EPPC(51),MOLC(51),       LN19050
     *               HWHMC(51),TMPALC(51),PSHIFC(51),IFGC(51),           LN19060
     *               MINDC(35),IOUTC(51)                                 LN19070
      COMMON /TRAC/ VNU2(40),STR2(40),ALF2(40),EPP2(40),MOL2(40),        LN19080
     *              HWHM2(40),TMPAL2(40),PSHIF2(40),IFG2(40),MIND2(35)   LN19090
      COMMON /UNITS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2           LN19100
      COMMON /SREJ/ SR(64),SRD(64),TALF(64)                              LN19110
      COMMON /ICN/ ILIN3,NMAX,NBLOCK                                     LN19120
C                                                                        LN19130
      EQUIVALENCE (HID1(1),HDATE) , (HID1(2),HTIME)                      LN19140
C                                                                        LN19150
C     THE FOLLOWING VALUES FOR TALF(M) ARE BASED ON TABLE II OF          LN19160
C     ROTHMAN ET AL, 1987.                                               LN19170
C                                                                        LN19180
      DATA  CMOL(1), CMOL(2), CMOL(3), CMOL(4), CMOL(5), CMOL(6) /       LN19190
     *     '  H2O ','  CO2 ','   O3 ','  N2O ','   CO ','  CH4 ' /,      LN19200
     *      TALF(1), TALF(2), TALF(3), TALF(4), TALF(5), TALF(6) /       LN19210
     *         0.64,    0.25,    0.24,    0.25,    0.21,    0.29 /       LN19220
      DATA  CMOL(7), CMOL(8), CMOL(9),CMOL(10),CMOL(11),CMOL(12) /       LN19230
     *     '   O2 ','   NO ','  SO2 ','  NO2 ','  NH3 ','  HNO3' /,      LN19240
     *      TALF(7), TALF(8), TALF(9),TALF(10),TALF(11),TALF(12) /       LN19250
     *          0.5,     0.5,     0.5,     0.5,     0.5,     0.5 /       LN19260
      DATA CMOL(13),CMOL(14),CMOL(15),CMOL(16),CMOL(17),CMOL(18) /       LN19270
     *     '    OH','    HF','  HCL ','  HBR ','   HI ','  CLO ' /,      LN19280
     *     TALF(13),TALF(14),TALF(15),TALF(16),TALF(17),TALF(18) /       LN19290
     *          0.5,     0.5,    0.46,     0.5,     0.5,     0.5 /       LN19300
      DATA CMOL(19),CMOL(20),CMOL(21),CMOL(22),CMOL(23),CMOL(24) /       LN19310
     *     '  OCS ',' H2CO ',' HOCL ','   N2 ','  HCN ','CH3CL ' /,      LN19320
     *     TALF(19),TALF(20),TALF(21),TALF(22),TALF(23),TALF(24) /       LN19330
     *          0.5,     0.5,     0.5,     0.5,     0.5,     0.5 /       LN19340
      DATA CMOL(25),CMOL(26),CMOL(27),CMOL(28),CMOL(29),CMOL(30) /       LN19350
     *     ' H2O2 ',' C2H2 ',' C2H6 ','  PH3 ',' COF2 ','  SF6 ' /,      LN19360
     *     TALF(25),TALF(26),TALF(27),TALF(28),TALF(29),TALF(30) /       LN19370
     *          0.5,    0.25,     0.5,     0.5,     0.5,     0.5 /       LN19380
      DATA CMOL(31),CMOL(32),CMOL(33),CMOL(34),CMOL(35),CMOL(36) /       LN19390
     *     '  H2S ','HCOOH ','      ','      ','      ','      ' /,      LN19400
     *     TALF(31),TALF(32),TALF(33),TALF(34),TALF(35),TALF(36) /       LN19410
     *          0.5,     0.5,     0.0,     0.0,     0.0,     0.0 /       LN19420
C                                                                        LN19430
      DATA (CMOL(I),I=37,64) / 28*'      '/                              LN19440
      DATA (TALF(I),I=37,64) / 28*0.0 /                                  LN19450
C                                                                        LN19460
C     THE FOLLOWING DATA STATEMENTS CONTAIN THE DEFAULT REJECTION        LN19470
C     FOR EACH OF THE 32 POSSIBLE MOLECULES - THESE ARE BASED ON         LN19480
C     A LIMB VIEW OF A TROPICAL ATMOSPHERE GIVEN THE FOLLOWING           LN19490
C     EQUATION:                                                          LN19500
C                  S(M) = DPTMIN/(50.*W(M))                              LN19510
C                                      WHERE DPTMIN=0.0005               LN19520
C                                                                        LN19530
C                THIS GIVES:  S(M) = 1.E-5/W(M)                          LN19540
C                                                                        LN19550
C                NOTE: NO PROFILES ARE CURRENTLY AVAILABLE FOR           LN19560
C                      MOLECULES 29 THROUGH 32 SO DEFAULT                LN19570
C                      REJECTIONS ARE CURRENTLY SET TO ZERO.             LN19580
C                                                                        LN19590
C                    H2O        CO2         O3        N2O         CO     LN19600
      DATA SRD / 6.667E-31, 1.873E-29, 3.817E-26, 1.984E-26, 4.785E-26,  LN19610
C                                                                        LN19620
C                    CH4         O2         NO        SO2        NO2     LN19630
     *           3.745E-27, 2.959E-32, 1.898E-23, 3.077E-23, 6.098E-23,  LN19640
C                                                                        LN19650
C                    NH3       HNO3         OH         HF        HCL     LN19660
     *           1.799E-23, 2.950E-23, 6.944E-21, 1.541E-21, 1.100E-23,  LN19670
C                                                                        LN19680
C                    HBR         HI        CLO        OCS       H2CO     LN19690
     *           3.623E-21, 2.062E-21, 3.436E-21, 1.110E-23, 7.246E-24,  LN19700
C                                                                        LN19710
C                   HOCL         N2        HCN      CH3CL       H2O2     LN19720
     *           5.650E-22, 7.937E-33, 3.802E-23, 9.804E-24, 3.731E-23,  LN19730
C                                                                        LN19740
C                   C2H2       C2H6        PH3       COF2        SF6     LN19750
     *           4.717E-23, 3.401E-24, 6.173E-13, 0.000E+00, 0.000E+00,  LN19760
C                                                                        LN19770
C                    H2S      HCOOH                                      LN19780
     *           0.000E+00, 0.999E+00, 32*0.0 /                          LN19790
C                                                                        LN19800
      DATA SR / 64*0.0 /                                                 LN19810
      DATA CHID08 / 'LNFL 2.7'/                                          LN19820
      DATA CHID10 / 'ISOTOP I'/                                          LN19820
      DATA IRD,IPR,IPU / 5,66,7 /                                        LN19830
      DATA SUMSTR / 64*0. /,MCNTLC / 64*0 /,ILINLC / 0 /,IREC / 0 /,     LN19840
     *     IRECTL / 0 /                                                  LN19850
      DATA MOLIND / 64*0 /,MIND1 / 35*0 /,MIND2 / 35*0 /                 LN19860
      DATA MINDC / 35*0 /,IOUT / 51*0 /,IOUTC / 51*0 /                   LN19870
      DATA MCNTNL / 64*0 /,ILINNL / 0 /                                  LN19880
      DATA PLANCK / 6.626176E-27 /,BOLTZ / 1.380662E-16 /,               LN19890
     *     CLIGHT / 2.99792458E10 /,AVOG / 6.022045E23 /                 LN19900
      DATA ILIN3 / 0 /,NMAX / 250 /,NBLOCK / 0 /                         LN19910
C                                                                        LN19920
      END                                                                LN19930
      BLOCK DATA CPLINS                                                  LN19940
C                                                                        LN19950
C**********************************************************************  LN19960
C                                                                        LN19970
C     THIS BLOCK DATA CONTAINS TRANSITION DATA INCLUDING LINE COUPLING   LN19980
C     PARAMETERS FOR REPLACEMENT OF SELECTED TRANSITIONS ON THE HITRAN   LN19990
C     DATABASE AFFECTED BY LINE COUPLING.                                LN20000
C                                                                        LN20010
C     THE FORMULATION IS DESCRIBED IN HOKE ET AL, 1988: PROC. OF THE     LN20020
C     INTERNATIONAL RADIATION  SYMPOSIUM, J. LENOBLE AND J.F. GELEYN,    LN20030
C     ED., A. DEEPAK PUB., 368-371.                                      LN20040
C                                                                        LN20050
C     THE LINE COUPLING DATA GENERALLY FOLLOWS THE DEVELOPMENT OF SMITH, LN20060
C            S' = S * ( 1. + G * (P/P0)**2 )           P0=1013 MB        LN20070
C            S''= S * (      Y * (P/P0)  )                               LN20080
C     VALUES FOR Y AND G ARE PROVIDED AT FOUR TEMPERATURES:              LN20090
C     200 K, 250 K, 296 K, AND 340 K                                     LN20100
C                                                                        LN20110
C**********************************************************************  LN20120
C                                                                        LN20130
C                    OXYGEN                                              LN20140
C                                                                        LN20150
C     A TRANSITION AT 0.000010 CM-1 HAS BEEN PROVIDED IN ORDER TO        LN20160
C     TREAT THE 'ZERO FREQUENCY' (NON-RESONANT) O2 BAND AS A SINGLE      LN20170
C     LINE WITH A PRESSURE DEPENDENT REFERENCE HALFWIDTH, REPLACING      LN20180
C     THE 'ZERO FREQUENCY' TRANSITIONS ON THE HITRAN DATA BASE FOR       LN20190
C     THE MAIN ISOTOPE.                                                  LN20200
C                                                                        LN20210
C     THE COUPLING COEFFICIENTS FOR THE 2 CM-1 (60 GHZ) FOR THE MAIN     LN20220
C     ISOTOPE ARE INCLUDED. THESE HAVE BEEN CALCULATED BY CLOUGH AND     LN20230
C     HOKE.                                                              LN20240
C                                                                        LN20250
C            THE O2 LINE COUPLING COEFFICIENTS WERE PROVIDED BY          LN20260
C            CLOUGH (1987). Y'S AND G'S WERE CALCULATED FOR OXYGEN       LN20270
C            USING A RELAXATION MATRIX WHICH WAS FIT TO SMITH'S          LN20280
C            HALFWIDTHS.                                                 LN20290
C                                                                        LN20300
C              NT        TEMP         X       A1            A2           LN20310
C                                                                        LN20320
C               1     200.0000     .7500  7.328597E-03  7.303265E-01     LN20330
C               2     250.0000     .7500  5.467282E-03  7.535262E-01     LN20340
C               3     296.0000     .7500  4.371108E-03  7.712720E-01     LN20350
C               4     340.0000     .7500  3.632896E-03  7.855163E-01     LN20360
C                                                                        LN20370
C         ALPHA(T) = ALPHA(TO)*(TO/T)**X                                 LN20380
C                                                                        LN20390
C         W(J,K) = A1*SQRT(RHO(J)/RHO(K))*EXP(-A2*BETA*ABS(E(J)-E(K)))   LN20400
C                                                                        LN20410
C**********************************************************************  LN20600
C                                                                        LN20610
C                   CARBON DIOXIDE                                       LN20620
C                                                                        LN20630
C     LINE COUPLING PARAMETERS ARE PROVIDED FOR CO2 Q BRANCHES AT        LN20640
C     618 CM-1, 667 CM-1, 720 CM-1, 721 CM-1, 742 CM-1 AND 792 CM-1.     LN20650
C                                                                        LN20660
C   ...................................................................  LN20670
C   .                                                                 .  LN20680
C   .   THE LINE COUPLING COEFFICIENTS FOR CARBON DIOXIDE HAVE BEEN   .  LN20690
C   .   MULTIPLIED BY A FACTOR OF 1.3 TO PROVIDE AGREEMENT WITH THE   .  LN20700
C   .   THE HIS DOWNLOOKING DATA OF 14 APRIL 1986 AND WITH THE HIS    .  LN20710
C   .   UPLOOKING GAPEX DATA OF 1 NOV. 1988. (CLOUGH ET AL 1991).     .  LN20720
C   .                                                                 .  LN20730
C   ...................................................................  LN20740
C                                                                        LN20750
C                                                                        LN20760
C     WITH THE EXCEPTION OF THE TRANSITION WAVENUMBER VALUES,
C     THE CO2 PARAMETERS, INCLUDING THE LINE COUPLING COEFFICIENTS,      LN20770
C     ARE FROM AN INITIAL VERSION OF THE 1992 HITRAN LINE PARAMETERS,    LN20780
C     NOT FROM THE FINAL RELEASED VERSION. THERE ARE SMALL DIFFERENCES   LN20790
C     IN STRENGTH AND HALFWIDTH. THE WAVENUMBER VALUES ARE FROM HITRAN92.LN20800
C                                                                        LN20810
C     COUPLING COEFFICIENTS ARE FROM M.L. HOKE (1991).                   LN20820
C                                                                        LN20830
C**********************************************************************  LN20840
C                                                                        LN20850
      IMPLICIT CHARACTER*50 (C)                                          LN20860
      COMMON /CPLMOL/ MOLCPL(35),NCPL                                    LN20870
      COMMON /CLINES/ CPL001(16),CPL005(16),CPL009(16),CPL013(16),       LN20880
     +     CPL017(16),CPL021(16),CPL025(16),CPL029(16),CPL033(16),       LN20890
     +     CPL037(16),CPL041(16),CPL045(16),CPL049(16),CPL053(16),       LN20900
     +     CPL057(16),CPL061(16),CPL065(16),CPL069(16),CPL073(16),       LN20910
     +     CPL077(16),CPL081(16),CPL085(16),CPL089(16),CPL093(16),       LN20920
     +     CPL097(16),CPL101(16),CPL105(16),CPL109(16),CPL113(16),       LN20930
     +     CPL117(16),CPL121(16),CPL125(16),CPL129(16),CPL133(16),       LN20940
     +     CPL137(16),CPL141(16),CPL145(16),CPL149(16),CPL153(16),       LN20950
     +     CPL157(16),CPL161(16),CPL165(16),CPL169(16),CPL173(16),       LN20960
     +     CPL177(16),CPL181(16),CPL185(16),CPL189(16),CPL193(16),       LN20970
     +     CPL197(16),CPL201(16),CPL205(16),CPL209(16),CPL213(16),       LN20980
     +     CPL217(16),CPL221(16),CPL225(16),CPL229(16),CPL233(16),       LN20990
     +     CPL237(16),CPL241(16),CPL245(16),CPL249(16),CPL253(16),       LN21000
     +     CPL257(16),CPL261(16),CPL265(16),CPL269(16),CPL273(16),       LN21010
     +     CPL277(16),CPL281(16),CPL285(16),CPL289(16),CPL293(16),       LN21020
     +     CPL297(12)                                                    LN21030
C                                                                        LN21040
C     MOLCPL CONTAINS THE FLAGS FOR MOLECULE 2 AND 7                     LN21050
C                                                                        LN21060
      DATA MOLCPL/0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,                         LN21070
     1            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,                         LN21080
     2            0,0,0,0,0/                                             LN21090
C                                                                        LN21100
      DATA NCPL/598/
C
      DATA CPL001/                                                       LN21110
     1     ' 70     .000010 2.401E-35 0.000E+00.0340.0000     ',         LN21120
     1     '.0000 .00 .000000  0  0 O2 BAND<.007 CM-1000 0 0-3',         LN21140
     1     ' 7 5.000000E-01 0.0000E+00 5.000000E-01 0.0000E+00',         LN21150
     1     ' 5.000000E-01 0.0000E+00 5.000000E-01 0.0000E+00-3',         LN21160
     2     ' 71    1.666549 1.380E-29 1.254E-04.0350.0000 2230',         LN21170
     2     '.4250 .50 .000000  1  1         Q39R38   281 0 0-1',         LN21180
     2     ' 7 8.924857E-01-3.4892E-01 6.924627E-01-2.0601E-01',         LN21190
     2     ' 5.688864E-01-1.3623E-01 4.834488E-01-9.6352E-02-1',         LN21200
     3     ' 71    1.683622 3.870E-29 1.190E-04.0350.0000 2011',         LN21210
     3     '.2150 .50 .000000  1  1         Q37R36   281 0 0-1',         LN21220
     3     ' 7 9.043039E-01-3.6270E-01 6.922687E-01-2.1240E-01',         LN21230
     3     ' 5.612283E-01-1.3950E-01 4.707339E-01-9.7973E-02-1',         LN21240
     4     ' 71    1.700755 1.030E-28 1.127E-04.0350.0000 1803',         LN21250
     4     '.1800 .50 .000000  1  1         Q35R34   281 0 0-1',         LN21260
     4     ' 7 9.190365E-01-3.7600E-01 6.986458E-01-2.1856E-01',         LN21270
     4     ' 5.627367E-01-1.4274E-01 4.689508E-01-9.9771E-02-1'/         LN21280
      DATA CPL005/                                                       LN21290
     5     ' 71    1.717957 2.580E-28 1.064E-04.0350.0000 1606',         LN21300
     5     '.3530 .50 .000000  1  1         Q33R32   281 0 0-1',         LN21310
     5     ' 7 9.326389E-01-3.8966E-01 7.043062E-01-2.2447E-01',         LN21320
     5     ' 5.641640E-01-1.4553E-01 4.677300E-01-1.0104E-01-1',         LN21330
     6     ' 71    1.735240 6.091E-28 9.998E-05.0360.0000 1420',         LN21340
     6     '.7670 .50 .000000  1  1         Q31R30   281 0 0-1',         LN21350
     6     ' 7 9.444580E-01-4.0317E-01 7.082229E-01-2.2981E-01',         LN21360
     6     ' 5.640782E-01-1.4764E-01 4.652853E-01-1.0165E-01-1',         LN21370
     7     ' 71    1.752621 1.360E-27 9.362E-05.0370.0000 1246',         LN21380
     7     '.4520 .50 .000000  1  1         Q29R28   281 0 0-1',         LN21390
     7     ' 7 9.493652E-01-4.1348E-01 7.063451E-01-2.3262E-01',         LN21400
     7     ' 5.590405E-01-1.4775E-01 4.585635E-01-1.0063E-01-1',         LN21410
     8     ' 71    1.770118 2.850E-27 8.733E-05.0370.0000 1083',         LN21420
     8     '.4360 .50 .000000  1  1         Q27R26   281 0 0-1',         LN21430
     8     ' 7 9.503993E-01-4.2170E-01 7.007965E-01-2.3334E-01',         LN21440
     8     ' 5.506298E-01-1.4605E-01 4.487690E-01-9.8063E-02-1'/         LN21450
      DATA CPL009/                                                       LN21460
     9     ' 71    1.787760 5.630E-27 8.096E-05.0380.0000  931',         LN21470
     9     '.7450 .50 .000000  1  1         Q25R24   281 0 0-1',         LN21480
     9     ' 7 9.428061E-01-4.2289E-01 6.881167E-01-2.2908E-01',         LN21490
     9     ' 5.361520E-01-1.4064E-01 4.337149E-01-9.2651E-02-1',         LN21500
     *     ' 71    1.805581 1.050E-26 7.456E-05.0380.0000  791',         LN21510
     *     '.4050 .50 .000000  1  1         Q23R22   281 0 0-1',         LN21520
     *     ' 7 9.202023E-01-4.1182E-01 6.635457E-01-2.1687E-01',         LN21530
     *     ' 5.118276E-01-1.2966E-01 4.102879E-01-8.3146E-02-1',         LN21540
     1     ' 71    1.823633 1.830E-26 6.823E-05.0390.0000  662',         LN21550
     1     '.4370 .50 .000000  1  1         Q21R20   281 0 0-1',         LN21560
     1     ' 7 8.843798E-01-3.8724E-01 6.283812E-01-1.9608E-01',         LN21570
     1     ' 4.786604E-01-1.1277E-01 3.792939E-01-6.9365E-02-1',         LN21580
     2     ' 71    1.841987 3.000E-26 6.187E-05.0400.0000  544',         LN21590
     2     '.8630 .50 .000000  1  1         Q19R18   281 0 0-1',         LN21600
     2     ' 7 8.280709E-01-3.4168E-01 5.776944E-01-1.6305E-01',         LN21610
     2     ' 4.330330E-01-8.7940E-02 3.379553E-01-5.0135E-02-1'/         LN21620
      DATA CPL013/                                                       LN21630
     3     ' 71    1.860748 4.601E-26 5.552E-05.0410.0000  438',         LN21640
     3     '.7020 .50 .000000  1  1         Q17R16   281 0 0-1',         LN21650
     3     ' 7 7.452284E-01-2.6969E-01 5.070097E-01-1.1543E-01',         LN21660
     3     ' 3.713874E-01-5.4123E-02 2.833280E-01-2.5043E-02-1',         LN21670
     4     ' 71    1.876791 2.741E-26 3.900E-06.0560.0000    2',         LN21680
     4     '.0840 .50 .000000  1  1         Q 1P 2   281 0 0-1',         LN21690
     4     ' 7 7.891961E-01-3.9363E-01 6.662377E-01-2.8643E-01',         LN21700
     4     ' 5.834021E-01-2.2264E-01 5.217716E-01-1.7989E-01-1',         LN21710
     5     ' 71    1.880081 6.580E-26 4.915E-05.0420.0000  343',         LN21720
     5     '.9700 .50 .000000  1  1         Q15R14   281 0 0-1',         LN21730
     5     ' 7 6.305813E-01-1.6961E-01 4.129153E-01-5.3919E-02',         LN21740
     5     ' 2.912991E-01-1.2712E-02 2.135958E-01 4.3501E-03-1',         LN21750
     6     ' 71    1.900255 8.771E-26 4.277E-05.0420.0000  260',         LN21760
     6     '.6830 .50 .000000  1  1         Q13R12   281 0 0-1',         LN21770
     6     ' 7 4.771772E-01-4.6852E-02 2.909217E-01 1.5824E-02',         LN21780
     6     ' 1.895838E-01 3.1306E-02 1.263787E-01 3.3798E-02-1'/         LN21790
      DATA CPL017/                                                       LN21800
     7     ' 71    1.921746 1.080E-25 3.636E-05.0430.0000  188',         LN21810
     7     '.8530 .50 .000000  1  1         Q11R10   281 0 0-1',         LN21820
     7     ' 7 2.799733E-01 7.6199E-02 1.378573E-01 7.7961E-02',         LN21830
     7     ' 6.402178E-02 6.6243E-02 2.002633E-02 5.4409E-02-1',         LN21840
     8     ' 71    1.945475 1.220E-25 2.998E-05.0450.0000  128',         LN21850
     8     '.4920 .50 .000000  1  1         Q 9R 8   281 0 0-1',         LN21860
     8     ' 7 4.274984E-02 1.5040E-01-4.227637E-02 1.0255E-01',         LN21870
     8     '-8.153185E-02 7.2106E-02-1.018404E-01 5.2079E-02-1',         LN21880
     9     ' 71    1.949568 7.550E-26 1.067E-05.0500.0000   16',         LN21890
     9     '.3880 .50 .000000  1  1         Q 3P 4   281 0 0-1',         LN21900
     9     ' 7 6.539178E-01-1.6373E-01 5.652978E-01-1.4228E-01',         LN21910
     9     ' 5.016129E-01-1.2159E-01 4.523904E-01-1.0439E-01-1',         LN21920
     *     ' 71    1.973506 1.260E-25 2.357E-05.0460.0000   79',         LN21930
     *     '.6070 .50 .000000  1  1         Q 7R 6   281 0 0-1',         LN21940
     *     ' 7-2.151828E-01 1.1201E-01-2.333207E-01 5.6423E-02',         LN21950
     *     '-2.331593E-01 2.9434E-02-2.270025E-01 1.4610E-02-1'/         LN21960
      DATA CPL021/                                                       LN21970
     1     ' 71    1.987741 1.110E-25 1.718E-05.0470.0000   42',         LN21980
     1     '.2240 .50 .000000  1  1         Q 5P 6   281 0 0-1',         LN21990
     1     ' 7 4.029483E-01 1.0244E-01 3.817892E-01 3.5235E-02',         LN22000
     1     ' 3.569014E-01 6.7936E-03 3.332993E-01-6.8670E-03-1',         LN22010
     2     ' 71    2.011594 1.130E-25 1.706E-05.0480.0000   42',         LN22020
     2     '.2000 .50 .000000  1  1         Q 5R 4   281 0 0-1',         LN22030
     2     ' 7-4.252801E-01-3.5516E-02-3.834073E-01-4.6308E-02',         LN22040
     2     '-3.490492E-01-4.6417E-02-3.205266E-01-4.3639E-02-1',         LN22050
     3     ' 71    2.015887 1.310E-25 2.361E-05.0450.0000   79',         LN22060
     3     '.5650 .50 .000000  1  1         Q 7P 8   281 0 0-1',         LN22070
     3     ' 7 1.116893E-01 2.1279E-01 1.646953E-01 1.2948E-01',         LN22080
     3     ' 1.835418E-01 8.4252E-02 1.893562E-01 5.7001E-02-1',         LN22090
     4     ' 71    2.039763 1.350E-25 3.002E-05.0440.0000  128',         LN22100
     4     '.3980 .50 .000000  1  1         Q 9P10   281 0 0-1',         LN22110
     4     ' 7-1.573533E-01 1.6248E-01-3.936570E-02 1.2520E-01',         LN22120
     4     ' 1.865285E-02 9.4901E-02 5.122895E-02 7.2789E-02-1'/         LN22130
      DATA CPL025/                                                       LN22140
     5     ' 71    2.061432 1.250E-25 3.640E-05.0430.0000  188',         LN22150
     5     '.7140 .50 .000000  1  1         Q11P12   281 0 0-1',         LN22160
     5     ' 7-3.818469E-01 3.2656E-02-2.127175E-01 6.4055E-02',         LN22170
     5     '-1.231166E-01 6.3182E-02-6.860509E-02 5.5971E-02-1',         LN22180
     6     ' 71    2.081815 1.050E-25 4.277E-05.0420.0000  260',         LN22190
     6     '.5010 .50 .000000  1  1         Q13P14   281 0 0-1',         LN22200
     6     ' 7-5.594329E-01-1.1019E-01-3.524158E-01-1.4259E-02',         LN22210
     6     '-2.387695E-01 1.5303E-02-1.672543E-01 2.4924E-02-1',         LN22220
     7     ' 71    2.084317 8.410E-26 1.040E-05.0500.0000   16',         LN22230
     7     '.2530 .50 .000000  1  1         Q 3R 2   281 0 0-1',         LN22240
     7     ' 7-4.501936E-01-1.1614E-01-3.871616E-01-8.9810E-02',         LN22250
     7     '-3.431207E-01-7.2485E-02-3.095733E-01-6.0146E-02-1',         LN22260
     8     ' 71    2.101387 8.230E-26 4.915E-05.0410.0000  343',         LN22270
     8     '.7480 .50 .000000  1  1         Q15P16   281 0 0-1',         LN22280
     8     ' 7-6.891305E-01-2.2935E-01-4.577648E-01-8.5784E-02',         LN22290
     8     '-3.278442E-01-3.1930E-02-2.444369E-01-8.0203E-03-1'/         LN22300
      DATA CPL029/                                                       LN22310
     9     ' 71    2.120418 5.971E-26 5.552E-05.0410.0000  438',         LN22320
     9     '.4420 .50 .000000  1  1         Q17P18   281 0 0-1',         LN22330
     9     ' 7-7.870405E-01-3.1845E-01-5.397957E-01-1.4314E-01',         LN22340
     9     '-3.985749E-01-7.1872E-02-3.066017E-01-3.7175E-02-1',         LN22350
     *     ' 71    2.139073 4.040E-26 6.187E-05.0400.0000  544',         LN22360
     *     '.5660 .50 .000000  1  1         Q19P20   281 0 0-1',         LN22370
     *     ' 7-8.555103E-01-3.7747E-01-5.999220E-01-1.8448E-01',         LN22380
     *     '-4.519122E-01-1.0228E-01-3.544307E-01-6.0350E-02-1',         LN22390
     1     ' 71    2.157456 2.559E-26 6.825E-05.0390.0000  662',         LN22400
     1     '.1030 .50 .000000  1  1         Q21P22   281 0 0-1',         LN22410
     1     ' 7-8.995018E-01-4.1028E-01-6.414460E-01-2.1075E-01',         LN22420
     1     '-4.902625E-01-1.2305E-01-3.897700E-01-7.6986E-02-1',         LN22430
     2     ' 71    2.175641 1.520E-26 7.458E-05.0380.0000  791',         LN22440
     2     '.0350 .50 .000000  1  1         Q23P24   281 0 0-1',         LN22450
     2     ' 7-9.269214E-01-4.2522E-01-6.701264E-01-2.2610E-01',         LN22460
     2     '-5.181402E-01-1.3649E-01-4.163089E-01-8.8433E-02-1'/         LN22470
      DATA CPL033/                                                       LN22480
     3     ' 71    2.193676 8.490E-27 8.096E-05.0380.0000  931',         LN22490
     3     '.3390 .50 .000000  1  1         Q25P26   281 0 0-1',         LN22500
     3     ' 7-9.389593E-01-4.2574E-01-6.866518E-01-2.3222E-01',         LN22510
     3     '-5.359680E-01-1.4352E-01-4.343095E-01-9.5196E-02-1',         LN22520
     4     ' 71    2.211598 4.450E-27 8.731E-05.0380.0000 1082',         LN22530
     4     '.9940 .50 .000000  1  1         Q27P28   281 0 0-1',         LN22540
     4     ' 7-9.425673E-01-4.1888E-01-6.961387E-01-2.3302E-01',         LN22550
     4     '-5.477750E-01-1.4658E-01-4.470707E-01-9.8912E-02-1',         LN22560
     5     ' 71    2.229434 2.200E-27 9.364E-05.0370.0000 1245',         LN22570
     5     '.9750 .50 .000000  1  1         Q29P30   281 0 0-1',         LN22580
     5     ' 7-9.376011E-01-4.0642E-01-6.985705E-01-2.2967E-01',         LN22590
     5     '-5.535956E-01-1.4649E-01-4.546516E-01-1.0017E-01-1',         LN22600
     6     ' 71    2.247204 1.020E-27 1.000E-04.0360.0000 1420',         LN22610
     6     '.2550 .50 .000000  1  1         Q31P32   281 0 0-1',         LN22620
     6     ' 7-9.288488E-01-3.9246E-01-6.973291E-01-2.2455E-01',         LN22630
     6     '-5.559941E-01-1.4478E-01-4.590827E-01-1.0001E-01-1'/         LN22640
      DATA CPL037/                                                       LN22650
     7     ' 71    2.264923 4.481E-28 1.063E-04.0350.0000 1605',         LN22660
     7     '.8060 .50 .000000  1  1         Q33P34   281 0 0-1',         LN22670
     7     ' 7-9.132156E-01-3.7567E-01-6.903146E-01-2.1711E-01',         LN22680
     7     '-5.534508E-01-1.4118E-01-4.592368E-01-9.8304E-02-1',         LN22690
     8     ' 71    2.282604 1.850E-28 1.127E-04.0350.0000 1802',         LN22700
     8     '.5980 .50 .000000  1  1         Q35P36   281 0 0-1',         LN22710
     8     ' 7-8.975705E-01-3.6019E-01-6.828014E-01-2.0992E-01',         LN22720
     8     '-5.503310E-01-1.3743E-01-4.588986E-01-9.6291E-02-1',         LN22730
     9     ' 71    2.300257 7.241E-29 1.190E-04.0350.0000 2010',         LN22740
     9     '.5990 .50 .000000  1  1         Q37P38   281 0 0-1',         LN22750
     9     ' 7-8.779950E-01-3.4357E-01-6.725431E-01-2.0165E-01',         LN22760
     9     '-5.455456E-01-1.3271E-01-4.578256E-01-9.3397E-02-1',         LN22770
     *     ' 71    2.317889 2.670E-29 1.253E-04.0350.0000 2229',         LN22780
     *     '.7740 .50 .000000  1  1         Q39P40   281 0 0-1',         LN22790
     *     ' 7-8.661057E-01-3.2937E-01-6.722827E-01-1.9482E-01',         LN22800
     *     '-5.525240E-01-1.2906E-01-4.697161E-01-9.1441E-02-1'/         LN22810
      DATA CPL041/                                                       LN22820
     1     ' 71    3.961085 1.000E-25 3.179E-06.0540.0000     ',         LN22830
     1     '.0000 .50 .000000  1  1         Q 1R 0   281 0 0-1',         LN22840
     1     ' 7-5.076856E-02-1.5892E-03-4.500990E-02-1.2656E-03',         LN22850
     1     '-4.091048E-02-1.0538E-03-3.773358E-02-9.0144E-04-1',         LN22860
     2     ' 21  612.195953 1.311E-25 1.761E+00.0559.0606 3196',         LN22870
     2     '.99760.75 0.00000  3  2             Q 80 423    -1',         LN22880
     2     ' 2 8.954269E-02-3.2300E-03 7.303400E-02-2.1646E-03',         LN22890
     2     ' 6.252350E-02-1.5965E-03 5.478330E-02-1.2325E-03-1',         LN22900
     3     ' 21  612.518329 2.394E-25 1.761E+00.0559.0610 3073',         LN22910
     3     '.12550.75 0.00000  3  2             Q 78 423    -1',         LN22920
     3     ' 2 7.751120E-02-2.3153E-03 6.083089E-02-1.4004E-03',         LN22930
     3     ' 5.065710E-02-9.5481E-04 4.343170E-02-6.9090E-04-1',         LN22940
     4     ' 21  612.829234 4.300E-25 1.757E+00.0559.0612 2952',         LN22950
     4     '.34320.75 0.00000  3  2             Q 76 423    -1',         LN22960
     4     ' 2 7.815340E-02-2.4218E-03 6.037200E-02-1.4342E-03',         LN22970
     4     ' 4.959500E-02-9.5802E-04 4.201600E-02-6.7971E-04-1'/         LN22980
      DATA CPL045/                                                       LN22990
     5     ' 21  613.128890 7.597E-25 1.751E+00.0559.0614 2834',         LN23000
     5     '.65270.75 0.00000  3  2             Q 74 423    -1',         LN23010
     5     ' 2 8.068450E-02-2.5909E-03 6.190730E-02-1.5231E-03',         LN23020
     5     ' 5.052580E-02-1.0101E-03 4.254120E-02-7.1157E-04-1',         LN23030
     6     ' 21  613.417516 1.320E-24 1.742E+00.0559.0615 2720',         LN23040
     6     '.05610.75 0.00000  3  2             Q 72 423    -1',         LN23050
     6     ' 2 8.379930E-02-2.7565E-03 6.407180E-02-1.6098E-03',         LN23060
     6     ' 5.210920E-02-1.0611E-03 4.372420E-02-7.4321E-04-1',         LN23070
     7     ' 21  613.695334 2.256E-24 1.731E+00.0559.0617 2608',         LN23080
     7     '.55510.75 0.00000  3  2             Q 70 423    -1',         LN23090
     7     ' 2 8.704670E-02-2.7787E-03 6.640010E-02-1.5972E-03',         LN23100
     7     ' 5.388240E-02-1.0376E-03 4.511390E-02-7.1620E-04-1',         LN23110
     8     ' 21  613.962560 3.791E-24 1.717E+00.0565.0619 2500',         LN23120
     8     '.15170.75 0.00000  3  2             Q 68 422    -1',         LN23130
     8     ' 2 9.070879E-02-3.0975E-03 6.905990E-02-1.7927E-03',         LN23140
     8     ' 5.594290E-02-1.1729E-03 4.676360E-02-8.1602E-04-1'/         LN23150
      DATA CPL049/                                                       LN23160
     9     ' 21  614.219409 6.266E-24 1.700E+00.0571.0623 2394',         LN23170
     9     '.84770.75 0.00000  3  2             Q 66 422    -1',         LN23180
     9     ' 2 9.441249E-02-3.3709E-03 7.175349E-02-1.9478E-03',         LN23190
     9     ' 5.803980E-02-1.2734E-03 4.845490E-02-8.8577E-04-1',         LN23200
     *     ' 21  614.466093 1.018E-23 1.680E+00.0577.0628 2292',         LN23210
     *     '.64490.75 0.00000  3  2             Q 64 422    -1',         LN23220
     *     ' 2 9.832940E-02-3.6683E-03 7.460440E-02-2.1139E-03',         LN23230
     *     ' 6.026150E-02-1.3793E-03 5.025020E-02-9.5811E-04-1',         LN23240
     1     ' 21  614.702823 1.628E-23 1.658E+00.0583.0635 2193',         LN23250
     1     '.54490.75 0.00000  3  2             Q 62 422    -1',         LN23260
     1     ' 2 1.024543E-01-3.9944E-03 7.759180E-02-2.2942E-03',         LN23270
     1     ' 6.258460E-02-1.4932E-03 5.212609E-02-1.0353E-03-1',         LN23280
     2     ' 21  614.929804 2.558E-23 1.633E+00.0589.0643 2097',         LN23290
     2     '.54950.75 0.00000  3  2             Q 60 422    -1',         LN23300
     2     ' 2 1.067625E-01-4.3524E-03 8.070010E-02-2.4913E-03',         LN23310
     2     ' 6.499350E-02-1.6171E-03 5.406830E-02-1.1189E-03-1'/         LN23320
      DATA CPL053/                                                       LN23330
     3     ' 21  615.147240 3.952E-23 1.606E+00.0595.0654 2004',         LN23340
     3     '.66020.75 0.00000  3  2             Q 58 422    -1',         LN23350
     3     ' 2 1.115075E-01-4.7602E-03 8.413339E-02-2.7156E-03',         LN23360
     3     ' 6.766370E-02-1.7580E-03 5.622759E-02-1.2139E-03-1',         LN23370
     4     ' 21  615.355330 6.003E-23 1.576E+00.0601.0667 1914',         LN23380
     4     '.87860.75 0.00000  3  2             Q 56 422    -1',         LN23390
     4     ' 2 1.163968E-01-5.2006E-03 8.765250E-02-2.9557E-03',         LN23400
     4     ' 7.038720E-02-1.9076E-03 5.842200E-02-1.3140E-03-1',         LN23410
     5     ' 21  615.554268 8.964E-23 1.544E+00.0607.0682 1828',         LN23420
     5     '.20610.75 0.00000  3  2             Q 54 422    -1',         LN23430
     5     ' 2 1.216514E-01-5.6904E-03 9.142900E-02-3.2209E-03',         LN23440
     5     ' 7.330959E-02-2.0721E-03 6.077630E-02-1.4236E-03-1',         LN23450
     6     ' 21  615.744245 1.316E-22 1.509E+00.0613.0699 1744',         LN23460
     6     '.64440.75 0.00000  3  2             Q 52 422    -1',         LN23470
     6     ' 2 1.266785E-01-6.1987E-03 9.499230E-02-3.4927E-03',         LN23480
     6     ' 7.603310E-02-2.2386E-03 6.294730E-02-1.5333E-03-1'/         LN23490
      DATA CPL057/                                                       LN23500
     7     ' 21  615.925448 1.899E-22 1.472E+00.0619.0718 1664',         LN23510
     7     '.19460.75 0.00000  3  2             Q 50 422    -1',         LN23520
     7     ' 2 1.324180E-01-6.7996E-03 9.905870E-02-3.8133E-03',         LN23530
     7     ' 7.914010E-02-2.4348E-03 6.542380E-02-1.6623E-03-1',         LN23540
     8     ' 21  616.098056 2.692E-22 1.433E+00.0625.0739 1586',         LN23550
     8     '.85820.75 0.00000  3  2             Q 48 422    -1',         LN23560
     8     ' 2 1.386060E-01-7.4669E-03 1.034462E-01-4.1668E-03',         LN23570
     8     ' 8.249540E-02-2.6493E-03 6.810180E-02-1.8023E-03-1',         LN23580
     9     ' 21  616.262247 3.751E-22 1.392E+00.0631.0761 1512',         LN23590
     9     '.63650.75 0.00000  3  2             Q 46 422    -1',         LN23600
     9     ' 2 1.450670E-01-8.1895E-03 1.080053E-01-4.5438E-03',         LN23610
     9     ' 8.597030E-02-2.8748E-03 7.086560E-02-1.9475E-03-1',         LN23620
     *     ' 21  616.418192 5.136E-22 1.348E+00.0637.0784 1441',         LN23630
     *     '.53080.75 0.00000  3  2             Q 44 422    -1',         LN23640
     *     ' 2 1.515800E-01-8.9517E-03 1.125384E-01-4.9325E-03',         LN23650
     *     ' 8.938409E-02-3.1019E-03 7.355530E-02-2.0901E-03-1'/         LN23660
      DATA CPL061/                                                       LN23670
     1     ' 21  616.566055 6.907E-22 1.303E+00.0643.0808 1373',         LN23680
     1     '.54210.75 0.00000  3  2             Q 42 422    -1',         LN23690
     1     ' 2 1.587170E-01-9.7834E-03 1.175005E-01-5.3453E-03',         LN23700
     1     ' 9.311770E-02-3.3361E-03 7.649330E-02-2.2322E-03-1',         LN23710
     2     ' 21  616.705995 9.124E-22 1.255E+00.0649.0833 1308',         LN23720
     2     '.67160.75 0.00000  3  2             Q 40 421    -1',         LN23730
     2     ' 2 1.661140E-01-1.0442E-02 1.225822E-01-5.6177E-03',         LN23740
     2     ' 9.690069E-02-3.4550E-03 7.944300E-02-2.2782E-03-1',         LN23750
     3     ' 21  616.838168 1.184E-21 1.205E+00.0657.0859 1246',         LN23760
     3     '.92050.75 0.00000  3  2             Q 38 421    -1',         LN23770
     3     ' 2 1.746810E-01-1.1858E-02 1.285011E-01-6.3814E-03',         LN23780
     3     ' 1.013363E-01-3.9295E-03 8.292440E-02-2.5977E-03-1',         LN23790
     4     ' 21  616.962718 1.507E-21 1.154E+00.0664.0885 1188',         LN23800
     4     '.28970.75 0.00000  3  2             Q 36 421    -1',         LN23810
     4     ' 2 1.823640E-01-1.2615E-02 1.336530E-01-6.6617E-03',         LN23820
     4     ' 1.050985E-01-4.0273E-03 8.580520E-02-2.6133E-03-1'/         LN23830
      DATA CPL065/                                                       LN23840
     5     ' 21  617.079789 1.884E-21 1.101E+00.0672.0910 1132',         LN23850
     5     '.78030.75 0.00000  3  2             Q 34 421    -1',         LN23860
     5     ' 2 1.916330E-01-1.3779E-02 1.399060E-01-7.1695E-03',         LN23870
     5     ' 1.096680E-01-4.2719E-03 8.931649E-02-2.7320E-03-1',         LN23880
     6     ' 21  617.189514 2.309E-21 1.046E+00.0681.0935 1080',         LN23890
     6     '.39310.75 0.00000  3  2             Q 32 421    -1',         LN23900
     6     ' 2 2.006550E-01-1.5210E-02 1.458600E-01-7.8220E-03',         LN23910
     6     ' 1.139437E-01-4.6086E-03 9.254440E-02-2.9156E-03-1',         LN23920
     7     ' 21  617.292021 2.777E-21 9.899E-01.0690.0960 1031',         LN23930
     7     '.12910.75 0.00000  3  2             Q 30 421    -1',         LN23940
     7     ' 2 2.104830E-01-1.6540E-02 1.522950E-01-8.3248E-03',         LN23950
     7     ' 1.185314E-01-4.7971E-03 9.598680E-02-2.9644E-03-1',         LN23960
     8     ' 21  617.387431 3.272E-21 9.320E-01.0699.0983  984',         LN23970
     8     '.98900.75 0.00000  3  2             Q 28 421    -1',         LN23980
     8     ' 2 2.208050E-01-1.7129E-02 1.588860E-01-8.2408E-03',         LN23990
     8     ' 1.231139E-01-4.5128E-03 9.934080E-02-2.6257E-03-1'/         LN24000
      DATA CPL069/                                                       LN24010
     9     ' 21  617.475857 3.777E-21 8.727E-01.0710.1005  941',         LN24020
     9     '.97360.75 0.00000  3  2             Q 26 421    -1',         LN24030
     9     ' 2 2.310230E-01-1.8751E-02 1.652040E-01-8.7888E-03',         LN24040
     9     ' 1.273727E-01-4.6726E-03 1.023555E-01-2.6247E-03-1',         LN24050
     *     ' 21  617.557408 4.268E-21 8.121E-01.0721.1026  902',         LN24060
     *     '.08370.75 0.00000  3  2             Q 24 421    -1',         LN24070
     *     ' 2 2.423590E-01-1.9111E-02 1.721460E-01-8.3274E-03',         LN24080
     *     ' 1.319890E-01-4.0173E-03 1.055834E-01-1.9557E-03-1',         LN24090
     1     ' 21  617.632182 4.715E-21 7.503E-01.0733.1045  865',         LN24100
     1     '.31990.75 0.00000  3  2             Q 22 421    -1',         LN24110
     1     ' 2 2.521350E-01-1.8187E-02 1.774890E-01-6.8917E-03',         LN24120
     1     ' 1.350830E-01-2.6047E-03 1.073800E-01-6.8557E-04-1',         LN24130
     2     ' 21  617.700272 5.088E-21 6.874E-01.0747.1063  831',         LN24140
     2     '.68290.75 0.00000  3  2             Q 20 421    -1',         LN24150
     2     ' 2 2.624700E-01-1.7969E-02 1.828840E-01-5.7630E-03',         LN24160
     2     ' 1.379820E-01-1.3615E-03 1.088906E-01 4.7098E-04-1'/         LN24170
      DATA CPL073/                                                       LN24180
     3     ' 21  617.761763 5.352E-21 6.234E-01.0761.1078  801',         LN24190
     3     '.17310.75 0.00000  3  2             Q 18 421    -1',         LN24200
     3     ' 2 2.716090E-01-1.2322E-02 1.868230E-01-7.7059E-04',         LN24210
     3     ' 1.393860E-01 2.8036E-03 1.089348E-01 3.9222E-03-1',         LN24220
     4     ' 21  617.816731 5.478E-21 5.585E-01.0777.1092  773',         LN24230
     4     '.79110.75 0.00000  3  2             Q 16 421    -1',         LN24240
     4     ' 2 2.785250E-01-4.9414E-03 1.883700E-01 5.5132E-03',         LN24250
     4     ' 1.384500E-01 7.9239E-03 1.067625E-01 8.0865E-03-1',         LN24260
     5     ' 21  617.865247 5.438E-21 4.927E-01.0793.1103  749',         LN24270
     5     '.53740.75 0.00000  3  2             Q 14 421    -1',         LN24280
     5     ' 2 2.786550E-01 1.6385E-02 1.836640E-01 2.1152E-02',         LN24290
     5     ' 1.317940E-01 1.9850E-02 9.937979E-02 1.7450E-02-1',         LN24300
     6     ' 21  617.907372 5.213E-21 4.262E-01.0810.1113  728',         LN24310
     6     '.41230.75 0.00000  3  2             Q 12 421    -1',         LN24320
     6     ' 2 2.673190E-01 5.5714E-02 1.687270E-01 4.8269E-02',         LN24330
     6     ' 1.159626E-01 3.9797E-02 8.371739E-02 3.2739E-02-1'/         LN24340
      DATA CPL077/                                                       LN24350
     7     ' 21  617.943160 4.793E-21 3.590E-01.0826.1121  710',         LN24360
     7     '.41630.75 0.00000  3  2             Q 10 421    -1',         LN24370
     7     ' 2 2.343900E-01 1.6074E-01 1.352000E-01 1.1706E-01',         LN24380
     7     ' 8.376029E-02 8.9060E-02 5.347550E-02 6.9972E-02-1',         LN24390
     8     ' 21  617.972657 4.181E-21 2.913E-01.0843.1129  695',         LN24400
     8     '.54960.75 0.00000  3  2             Q  8 421    -1',         LN24410
     8     ' 2 1.443000E-01 4.0498E-01 5.435950E-02 2.6988E-01',         LN24420
     8     ' 1.086917E-02 1.9534E-01-1.250730E-02 1.4877E-01-1',         LN24430
     9     ' 21  617.995901 3.392E-21 2.232E-01.0860.1136  683',         LN24440
     9     '.81240.75 0.00000  3  2             Q  6 421    -1',         LN24450
     9     ' 2-8.047130E-02 1.1181E+00-1.357070E-01 7.0037E-01',         LN24460
     9     '-1.547650E-01 4.8794E-01-1.593150E-01 3.6276E-01-1',         LN24470
     *     ' 21  618.012923 2.452E-21 1.547E-01.0876.1146  675',         LN24480
     *     '.20500.75 0.00000  3  2             Q  4 421    -1',         LN24490
     *     ' 2-8.516560E-01 3.6551E+00-7.574060E-01 2.1692E+00',         LN24500
     *     '-6.807320E-01 1.4582E+00-6.170970E-01 1.0603E+00-1'/         LN24510
      DATA CPL081/                                                       LN24520
     1     ' 21  618.023745 1.400E-21 8.604E-02.0892.1173  669',         LN24530
     1     '.72740.75 0.00000  3  2             Q  2 421    -1',         LN24540
     1     ' 2-5.640050E+00-1.0555E+01-4.470570E+00-6.7582E+00',         LN24550
     1     '-3.743090E+00-4.8014E+00-3.241810E+00-3.6357E+00-1',         LN24560
     2     ' 21  667.386174 7.477E-20 8.239E-02.0892.1173    2',         LN24570
     2     '.34130.75 0.00000  2  1             Q  2 421    -1',         LN24580
     2     ' 2 4.205760E+00-5.8766E+00 3.334370E+00-3.7640E+00',         LN24590
     2     ' 2.792270E+00-2.6750E+00 2.418650E+00-2.0261E+00-1',         LN24600
     3     ' 21  667.400674 1.311E-19 1.483E-01.0876.1146    7',         LN24610
     3     '.80430.75 0.00000  2  1             Q  4 421    -1',         LN24620
     3     ' 2 6.460220E-01 2.0171E+00 5.739110E-01 1.1961E+00',         LN24630
     3     ' 5.155800E-01 8.0354E-01 4.672850E-01 5.8395E-01-1',         LN24640
     4     ' 21  667.423457 1.816E-19 2.142E-01.0860.1136   16',         LN24650
     4     '.38900.75 0.00000  2  1             Q  6 421    -1',         LN24660
     4     ' 2 7.248670E-02 6.1975E-01 1.115361E-01 3.8769E-01',         LN24670
     4     ' 1.243710E-01 2.6980E-01 1.267903E-01 2.0041E-01-1'/         LN24680
      DATA CPL085/                                                       LN24690
     5     ' 21  667.454520 2.243E-19 2.801E-01.0843.1129   28',         LN24700
     5     '.09510.75 0.00000  2  1             Q  8 421    -1',         LN24710
     5     ' 2-1.004549E-01 2.2759E-01-3.422380E-02 1.5140E-01',         LN24720
     5     '-2.396290E-03 1.0944E-01 1.456260E-02 8.3265E-02-1',         LN24730
     6     ' 21  667.493860 2.579E-19 3.460E-01.0826.1121   42',         LN24740
     6     '.92250.75 0.00000  2  1             Q 10 421    -1',         LN24750
     6     ' 2-1.684020E-01 9.1130E-02-9.516520E-02 6.6109E-02',         LN24760
     6     '-5.732610E-02 5.0180E-02-3.515200E-02 3.9360E-02-1',         LN24770
     7     ' 21  667.541472 2.814E-19 4.119E-01.0810.1113   60',         LN24780
     7     '.87090.75 0.00000  2  1             Q 12 421    -1',         LN24790
     7     ' 2-1.944020E-01 3.2209E-02-1.212549E-01 2.7599E-02',         LN24800
     7     '-8.221719E-02 2.2641E-02-5.843890E-02 1.8568E-02-1',         LN24810
     8     ' 21  667.597348 2.946E-19 4.778E-01.0793.1103   81',         LN24820
     8     '.94010.75 0.00000  2  1             Q 14 421    -1',         LN24830
     8     ' 2-2.023580E-01 1.0175E-02-1.319890E-01 1.2425E-02',         LN24840
     8     '-9.368060E-02 1.1489E-02-6.981650E-02 1.0026E-02-1'/         LN24850
      DATA CPL089/                                                       LN24860
     9     ' 21  667.661483 2.981E-19 5.437E-01.0777.1092  106',         LN24870
     9     '.12970.75 0.00000  2  1             Q 16 421    -1',         LN24880
     9     ' 2-2.020850E-01-1.5421E-03-1.352780E-01 3.8432E-03',         LN24890
     9     '-9.840609E-02 4.9509E-03-7.508410E-02 4.8970E-03-1',         LN24900
     *     ' 21  667.733869 2.927E-19 6.097E-01.0761.1078  133',         LN24910
     *     '.43930.75 0.00000  2  1             Q 18 421    -1',         LN24920
     *     ' 2-1.994460E-01-5.9959E-03-1.361360E-01 1.7673E-04',         LN24930
     *     '-1.008046E-01 2.0098E-03-7.817810E-02 2.5282E-03-1',         LN24940
     1     ' 21  667.814495 2.798E-19 6.756E-01.0747.1063  163',         LN24950
     1     '.86840.75 0.00000  2  1             Q 20 421    -1',         LN24960
     1     ' 2-1.928420E-01-9.3341E-03-1.333150E-01-2.7214E-03',         LN24970
     1     '-9.982570E-02-3.8948E-04-7.818460E-02 5.4867E-04-1',         LN24980
     2     ' 21  667.903351 2.610E-19 7.415E-01.0733.1045  197',         LN24990
     2     '.41660.75 0.00000  2  1             Q 22 421    -1',         LN25000
     2     ' 2-1.866150E-01-9.6885E-03-1.304550E-01-3.4759E-03',         LN25010
     2     '-9.861150E-02-1.1582E-03-7.787260E-02-1.4468E-04-1'/         LN25020
      DATA CPL093/                                                       LN25030
     3     ' 21  668.000428 2.378E-19 8.074E-01.0721.1026  234',         LN25040
     3     '.08330.75 0.00000  2  1             Q 24 421    -1',         LN25050
     3     ' 2-1.791920E-01-1.0275E-02-1.263314E-01-4.3234E-03',         LN25060
     3     '-9.618180E-02-1.9794E-03-7.641270E-02-8.7781E-04-1',         LN25070
     4     ' 21  668.105711 2.120E-19 8.733E-01.0710.1005  273',         LN25080
     4     '.86800.75 0.00000  2  1             Q 26 421    -1',         LN25090
     4     ' 2-1.720940E-01-1.0276E-02-1.222013E-01-4.6981E-03',         LN25100
     4     '-9.359610E-02-2.4214E-03-7.473180E-02-1.3043E-03-1',         LN25110
     5     ' 21  668.219188 1.851E-19 9.392E-01.0699.0983  316',         LN25120
     5     '.76980.75 0.00000  2  1             Q 28 421    -1',         LN25130
     5     ' 2-1.647230E-01-9.4160E-03-1.176617E-01-4.4179E-03',         LN25140
     5     '-9.055280E-02-2.3481E-03-7.258680E-02-1.3149E-03-1',         LN25150
     6     ' 21  668.340844 1.585E-19 1.005E+00.0690.0960  362',         LN25160
     6     '.78830.75 0.00000  2  1             Q 30 421    -1',         LN25170
     6     ' 2-1.579110E-01-9.2258E-03-1.134523E-01-4.5527E-03',         LN25180
     6     '-8.772660E-02-2.5681E-03-7.059649E-02-1.5493E-03-1'/         LN25190
      DATA CPL097/                                                       LN25200
     7     ' 21  668.470664 1.330E-19 1.071E+00.0681.0935  411',         LN25210
     7     '.92250.75 0.00000  2  1             Q 32 421    -1',         LN25220
     7     ' 2-1.514890E-01-8.5960E-03-1.093404E-01-4.3399E-03',         LN25230
     7     '-8.486010E-02-2.5089E-03-6.849179E-02-1.5548E-03-1',         LN25240
     8     ' 21  668.608632 1.095E-19 1.137E+00.0672.0910  464',         LN25250
     8     '.17170.75 0.00000  2  1             Q 34 421    -1',         LN25260
     8     ' 2-1.452100E-01-7.8357E-03-1.053052E-01-4.0033E-03',         LN25270
     8     '-8.203260E-02-2.3413E-03-6.641180E-02-1.4677E-03-1',         LN25280
     9     ' 21  668.754730 8.858E-20 1.203E+00.0664.0885  519',         LN25290
     9     '.53500.75 0.00000  2  1             Q 36 421    -1',         LN25300
     9     ' 2-1.397630E-01-7.3280E-03-1.017640E-01-3.8054E-03',         LN25310
     9     '-7.953010E-02-2.2624E-03-6.455150E-02-1.4427E-03-1',         LN25320
     *     ' 21  668.908940 7.033E-20 1.269E+00.0657.0859  578',         LN25330
     *     '.01160.75 0.00000  2  1             Q 38 421    -1',         LN25340
     *     ' 2-1.340560E-01-6.9488E-03-9.794330E-02-3.6825E-03',         LN25350
     *     '-7.675070E-02-2.2344E-03-6.242990E-02-1.4555E-03-1'/         LN25360
      DATA CPL101/                                                       LN25370
     1     ' 21  669.071242 5.486E-20 1.335E+00.0649.0833  639',         LN25380
     1     '.60040.75 0.00000  2  1             Q 40 421    -1',         LN25390
     1     ' 2-1.286259E-01-6.1932E-03-9.428120E-02-3.2761E-03',         LN25400
     1     '-7.407010E-02-1.9820E-03-6.036940E-02-1.2854E-03-1',         LN25410
     2     ' 21  669.241615 4.205E-20 1.401E+00.0643.0808  704',         LN25420
     2     '.30050.75 0.00000  2  1             Q 42 422    -1',         LN25430
     2     ' 2-1.236196E-01-5.8899E-03-9.088949E-02-3.1684E-03',         LN25440
     2     '-7.157540E-02-1.9486E-03-5.844670E-02-1.2851E-03-1',         LN25450
     3     ' 21  669.420040 3.167E-20 1.466E+00.0637.0784  772',         LN25460
     3     '.11070.75 0.00000  2  1             Q 44 422    -1',         LN25470
     3     ' 2-1.190072E-01-5.4778E-03-8.775389E-02-2.9731E-03',         LN25480
     3     '-6.926270E-02-1.8435E-03-5.665920E-02-1.2253E-03-1',         LN25490
     4     ' 21  669.606492 2.345E-20 1.532E+00.0631.0761  843',         LN25500
     4     '.03010.75 0.00000  2  1             Q 46 422    -1',         LN25510
     4     ' 2-1.144598E-01-5.0631E-03-8.462349E-02-2.7664E-03',         LN25520
     4     '-6.692920E-02-1.7255E-03-5.483790E-02-1.1530E-03-1'/         LN25530
      DATA CPL105/                                                       LN25540
     5     ' 21  669.800948 1.707E-20 1.598E+00.0625.0739  917',         LN25550
     5     '.05730.75 0.00000  2  1             Q 48 422    -1',         LN25560
     5     ' 2-1.104857E-01-4.7008E-03-8.190909E-02-2.5843E-03',         LN25570
     5     '-6.492069E-02-1.6206E-03-5.328180E-02-1.0880E-03-1',         LN25580
     6     ' 21  670.003386 1.222E-20 1.664E+00.0619.0718  994',         LN25590
     6     '.19130.75 0.00000  2  1             Q 50 422    -1',         LN25600
     6     ' 2-1.065636E-01-4.3611E-03-7.917260E-02-2.4094E-03',         LN25610
     6     '-6.285500E-02-1.5172E-03-5.165160E-02-1.0223E-03-1',         LN25620
     7     ' 21  670.213778 8.607E-21 1.730E+00.0613.0699 1074',         LN25630
     7     '.43060.75 0.00000  2  1             Q 52 422    -1',         LN25640
     7     ' 2-1.026506E-01-4.0382E-03-7.643610E-02-2.2412E-03',         LN25650
     7     '-6.078669E-02-1.4166E-03-5.001880E-02-9.5759E-04-1',         LN25660
     8     ' 21  670.432098 5.961E-21 1.796E+00.0607.0682 1157',         LN25670
     8     '.77410.75 0.00000  2  1             Q 54 422    -1',         LN25680
     8     ' 2-9.913930E-02-3.7514E-03-7.397909E-02-2.0909E-03',         LN25690
     8     '-5.892770E-02-1.3261E-03-4.854980E-02-8.9899E-04-1'/         LN25700
      DATA CPL109/                                                       LN25710
     9     ' 21  670.658321 4.061E-21 1.862E+00.0601.0667 1244',         LN25720
     9     '.22040.75 0.00000  2  1             Q 56 422    -1',         LN25730
     9     ' 2-9.576450E-02-3.4891E-03-7.160140E-02-1.9523E-03',         LN25740
     9     '-5.712070E-02-1.2422E-03-4.711590E-02-8.4425E-04-1',         LN25750
     *     ' 21  670.892417 2.722E-21 1.928E+00.0595.0654 1333',         LN25760
     *     '.76790.75 0.00000  2  1             Q 58 422    -1',         LN25770
     *     ' 2-9.251189E-02-3.2469E-03-6.929129E-02-1.8230E-03',         LN25780
     *     '-5.535140E-02-1.1631E-03-4.570280E-02-7.9215E-04-1',         LN25790
     1     ' 21  671.134357 1.795E-21 1.994E+00.0589.0643 1426',         LN25800
     1     '.41530.75 0.00000  2  1             Q 60 422    -1',         LN25810
     1     ' 2-8.929700E-02-3.0166E-03-6.700329E-02-1.6991E-03',         LN25820
     1     '-5.359640E-02-1.0866E-03-4.430010E-02-7.4142E-04-1',         LN25830
     2     ' 21  671.384111 1.165E-21 2.060E+00.0583.0635 1522',         LN25840
     2     '.16110.75 0.00000  2  1             Q 62 422    -1',         LN25850
     2     ' 2-8.654879E-02-2.8198E-03-6.507410E-02-1.5938E-03',         LN25860
     2     '-5.213390E-02-1.0220E-03-4.314570E-02-6.9880E-04-1'/         LN25870
      DATA CPL113/                                                       LN25880
     3     ' 21  671.641650 7.439E-22 2.126E+00.0577.0628 1621',         LN25890
     3     '.00360.75 0.00000  2  1             Q 64 422    -1',         LN25900
     3     ' 2-8.402549E-02-2.6467E-03-6.328270E-02-1.5006E-03',         LN25910
     3     '-5.076370E-02-9.6453E-04-4.205240E-02-6.6063E-04-1',         LN25920
     4     ' 21  671.906940 4.675E-22 2.191E+00.0571.0623 1722',         LN25930
     4     '.94120.75 0.00000  2  1             Q 66 422    -1',         LN25940
     4     ' 2-8.132670E-02-2.4743E-03-6.134830E-02-1.4065E-03',         LN25950
     4     '-4.927130E-02-9.0559E-04-4.085380E-02-6.2094E-04-1',         LN25960
     5     ' 21  672.179949 2.891E-22 2.257E+00.0565.0619 1827',         LN25970
     5     '.97230.75 0.00000  2  1             Q 68 422    -1',         LN25980
     5     ' 2-7.890870E-02-2.3162E-03-5.961410E-02-1.3179E-03',         LN25990
     5     '-4.793360E-02-8.4871E-04-3.978000E-02-5.8159E-04-1',         LN26000
     6     ' 21  672.460644 1.760E-22 2.323E+00.0559.0617 1936',         LN26010
     6     '.09510.75 0.00000  2  1             Q 70 423    -1',         LN26020
     6     ' 2-7.644130E-02-2.1063E-03-5.782660E-02-1.1859E-03',         LN26030
     6     '-4.654260E-02-7.5527E-04-3.865420E-02-5.1119E-04-1'/         LN26040
      DATA CPL117/                                                       LN26050
     7     ' 21  672.748990 1.055E-22 2.389E+00.0559.0615 2047',         LN26060
     7     '.30790.75 0.00000  2  1             Q 72 423    -1',         LN26070
     7     ' 2-7.405320E-02-2.1436E-03-5.611060E-02-1.2308E-03',         LN26080
     7     '-4.521530E-02-7.9847E-04-3.758820E-02-5.5078E-04-1',         LN26090
     8     ' 21  673.044952 6.221E-23 2.455E+00.0559.0614 2161',         LN26100
     8     '.60880.75 0.00000  2  1             Q 74 423    -1',         LN26110
     8     ' 2-7.223839E-02-2.0723E-03-5.481840E-02-1.1982E-03',         LN26120
     8     '-4.422730E-02-7.8214E-04-3.680040E-02-5.4265E-04-1',         LN26130
     9     ' 21  673.348494 3.613E-23 2.521E+00.0559.0612 2278',         LN26140
     9     '.99590.75 0.00000  2  1             Q 76 423    -1',         LN26150
     9     ' 2-7.007780E-02-1.9686E-03-5.324280E-02-1.1430E-03',         LN26160
     9     '-4.299360E-02-7.4871E-04-3.579810E-02-5.2107E-04-1',         LN26170
     *     ' 21  673.659580 2.065E-23 2.587E+00.0559.0610 2399',         LN26180
     *     '.46740.75 0.00000  2  1             Q 78 423    -1',         LN26190
     *     ' 2-6.803810E-02-1.8629E-03-5.176730E-02-1.0855E-03',         LN26200
     *     '-4.184830E-02-7.1309E-04-3.487510E-02-4.9747E-04-1'/         LN26210
      DATA CPL121/                                                       LN26220
     1     ' 21  673.978170 1.162E-23 2.653E+00.0559.0606 2523',         LN26230
     1     '.02120.75 0.00000  2  1             Q 80 423    -1',         LN26240
     1     ' 2-6.634810E-02-1.7720E-03-5.054660E-02-1.0356E-03',         LN26250
     1     '-4.090320E-02-6.8199E-04-3.411590E-02-4.7671E-04-1',         LN26260
     2     ' 21  674.304228 6.439E-24 2.719E+00.0559.0606 2649',         LN26270
     2     '.65530.75 0.00000  2  1             Q 82 423    -1',         LN26280
     2     ' 2-6.442020E-02-1.6744E-03-4.913870E-02-9.8114E-04',         LN26290
     2     '-3.980340E-02-6.4739E-04-3.322800E-02-4.5323E-04-1',         LN26300
     3     ' 21  674.637713 3.513E-24 2.785E+00.0559.0606 2779',         LN26310
     3     '.36760.75 0.00000  2  1             Q 84 423    -1',         LN26320
     3     ' 2-6.275750E-02-1.5882E-03-4.793620E-02-9.3287E-04',         LN26330
     3     '-3.887650E-02-6.1665E-04-3.249090E-02-4.3230E-04-1',         LN26340
     4     ' 21  674.978586 1.887E-24 2.851E+00.0559.0606 2912',         LN26350
     4     '.15600.75 0.00000  2  1             Q 86 423    -1',         LN26360
     4     ' 2-6.104929E-02-1.5025E-03-4.671160E-02-8.8455E-04',         LN26370
     4     '-3.794830E-02-5.8568E-04-3.177070E-02-4.1110E-04-1'/         LN26380
      DATA CPL125/                                                       LN26390
     5     ' 21  675.326806 9.980E-25 2.916E+00.0559.0606 3048',         LN26400
     5     '.01830.75 0.00000  2  1             Q 88 423    -1',         LN26410
     5     ' 2-5.964400E-02-1.4288E-03-4.576129E-02-8.4310E-04',         LN26420
     5     '-3.728400E-02-5.5930E-04-3.130790E-02-3.9322E-04-1',         LN26430
     6     ' 21  675.682330 5.198E-25 2.982E+00.0559.0606 3186',         LN26440
     6     '.95230.75 0.00000  2  1             Q 90 423    -1',         LN26450
     6     ' 2-5.837780E-02-1.3575E-03-4.504630E-02-8.0400E-04',         LN26460
     6     '-3.691740E-02-5.3535E-04-3.118050E-02-3.7779E-04-1',         LN26470
     7     ' 21  676.045118 2.666E-25 3.048E+00.0559.0606 3328',         LN26480
     7     '.95560.75 0.00000  2  1             Q 92 423    -1',         LN26490
     7     ' 2-5.827900E-02-1.3118E-03-4.566510E-02-7.8963E-04',         LN26500
     7     '-3.794960E-02-5.3468E-04-3.246360E-02-3.8376E-04-1',         LN26510
     8     ' 21  676.415126 1.346E-25 3.114E+00.0559.0606 3474',         LN26520
     8     '.02600.75 0.00000  2  1             Q 94 423    -1',         LN26530
     8     ' 2-6.767929E-02-1.8326E-03-5.537999E-02-1.2352E-03',         LN26540
     8     '-4.752150E-02-9.1491E-04-4.170660E-02-7.0849E-04-1'/         LN26550
      DATA CPL129/                                                       LN26560
     9     ' 21  714.584876 1.569E-25 3.280E+00.0559.0606 3323',         LN26570
     9     '.95750.75 0.00000  5  2             Q 82 423    -1',         LN26580
     9     ' 2 1.043419E-01-4.3304E-03 8.500960E-02-2.8955E-03',         LN26590
     9     ' 7.270119E-02-2.1316E-03 6.365320E-02-1.6435E-03-1',         LN26600
     *     ' 21  714.833295 2.804E-25 3.161E+00.0559.0606 3196',         LN26610
     *     '.99760.75 0.00000  5  2             Q 80 423    -1',         LN26620
     *     ' 2 8.547109E-02-2.7180E-03 6.647290E-02-1.5905E-03',         LN26630
     *     ' 5.495750E-02-1.0532E-03 4.684420E-02-7.4217E-04-1',         LN26640
     1     ' 21  715.080629 4.935E-25 3.045E+00.0559.0610 3073',         LN26650
     1     '.12550.75 0.00000  5  2             Q 78 423    -1',         LN26660
     1     ' 2 8.333519E-02-2.6967E-03 6.357520E-02-1.5434E-03',         LN26670
     1     ' 5.167760E-02-9.9856E-04 4.338360E-02-6.8722E-04-1',         LN26680
     2     ' 21  715.326475 8.550E-25 2.932E+00.0559.0612 2952',         LN26690
     2     '.34320.75 0.00000  5  2             Q 76 423    -1',         LN26700
     2     ' 2 8.376680E-02-2.7561E-03 6.339970E-02-1.5692E-03',         LN26710
     2     ' 5.112770E-02-1.0101E-03 4.259060E-02-6.9147E-04-1'/         LN26720
      DATA CPL133/                                                       LN26730
     3     ' 22  715.521486 1.023E-25 1.162E+00.0577.0628 2273',         LN26740
     3     '.70200.75 0.00000  5  2             Q 64 422    -1',         LN26750
     3     ' 2 8.441290E-02-2.7048E-03 6.850740E-02-1.7945E-03',         LN26760
     3     ' 5.840900E-02-1.3125E-03 5.103280E-02-1.0071E-03-1',         LN26770
     4     ' 21  715.570440 1.459E-24 2.822E+00.0559.0614 2834',         LN26780
     4     '.65270.75 0.00000  5  2             Q 74 423    -1',         LN26790
     4     ' 2 8.474569E-02-2.8145E-03 6.390150E-02-1.5963E-03',         LN26800
     4     ' 5.132920E-02-1.0240E-03 4.258410E-02-6.9883E-04-1',         LN26810
     5     ' 21  715.812147 2.450E-24 2.715E+00.0559.0615 2720',         LN26820
     5     '.05610.75 0.00000  5  2             Q 72 423    -1',         LN26830
     5     ' 2 8.603010E-02-2.8502E-03 6.472050E-02-1.6059E-03',         LN26840
     5     ' 5.186350E-02-1.0241E-03 4.292340E-02-6.9491E-04-1',         LN26850
     6     ' 22  715.874133 1.605E-25 1.126E+00.0583.0635 2174',         LN26860
     6     '.60430.75 0.00000  5  2             Q 62 422    -1',         LN26870
     6     ' 2 6.773780E-02-1.5636E-03 5.272540E-02-9.0671E-04',         LN26880
     6     ' 4.365530E-02-5.9804E-04 3.728530E-02-4.2094E-04-1'/         LN26890
      DATA CPL137/                                                       LN26900
     7     ' 21  716.051234 4.053E-24 2.610E+00.0559.0617 2608',         LN26910
     7     '.55510.75 0.00000  5  2             Q 70 423    -1',         LN26920
     7     ' 2 8.750819E-02-2.7021E-03 6.572280E-02-1.4888E-03',         LN26930
     7     ' 5.258370E-02-9.2888E-04 4.345120E-02-6.1595E-04-1',         LN26940
     8     ' 22  716.217931 2.479E-25 1.090E+00.0589.0643 2078',         LN26950
     8     '.61110.75 0.00000  5  2             Q 60 422    -1',         LN26960
     8     ' 2 6.627010E-02-1.5855E-03 5.046990E-02-8.9114E-04',         LN26970
     8     ' 4.104360E-02-5.6980E-04 3.452020E-02-3.8905E-04-1',         LN26980
     9     ' 21  716.287349 6.599E-24 2.509E+00.0565.0619 2500',         LN26990
     9     '.15170.75 0.00000  5  2             Q 68 422    -1',         LN27000
     9     ' 2 8.909940E-02-2.9068E-03 6.682130E-02-1.6236E-03',         LN27010
     9     ' 5.339490E-02-1.0283E-03 4.407130E-02-6.9359E-04-1',         LN27020
     *     ' 21  716.520156 1.058E-23 2.409E+00.0571.0623 2394',         LN27030
     *     '.84770.75 0.00000  5  2             Q 66 422    -1',         LN27040
     *     ' 2 9.066200E-02-3.0329E-03 6.788860E-02-1.6936E-03',         LN27050
     *     ' 5.418010E-02-1.0735E-03 4.467190E-02-7.2532E-04-1'/         LN27060
      DATA CPL141/                                                       LN27070
     1     ' 22  716.552646 3.767E-25 1.054E+00.0595.0654 1985',         LN27080
     1     '.72400.75 0.00000  5  2             Q 58 422    -1',         LN27090
     1     ' 2 6.720480E-02-1.6767E-03 5.063760E-02-9.3556E-04',         LN27100
     1     ' 4.077580E-02-5.9370E-04 3.398590E-02-4.0228E-04-1',         LN27110
     2     ' 21  716.749330 1.669E-23 2.312E+00.0577.0628 2292',         LN27120
     2     '.64490.75 0.00000  5  2             Q 64 422    -1',         LN27130
     2     ' 2 9.255220E-02-3.1756E-03 6.919380E-02-1.7701E-03',         LN27140
     2     ' 5.515380E-02-1.1207E-03 4.542850E-02-7.5690E-04-1',         LN27150
     3     ' 22  716.878057 5.632E-25 1.018E+00.0601.0667 1895',         LN27160
     3     '.94460.75 0.00000  5  2             Q 56 422    -1',         LN27170
     3     ' 2 6.896890E-02-1.7884E-03 5.166329E-02-9.9438E-04',         LN27180
     3     ' 4.136470E-02-6.2911E-04 3.428880E-02-4.2514E-04-1',         LN27190
     4     ' 21  716.974559 2.592E-23 2.218E+00.0583.0635 2193',         LN27200
     4     '.54490.75 0.00000  5  2             Q 62 422    -1',         LN27210
     4     ' 2 9.464389E-02-3.3325E-03 7.064850E-02-1.8530E-03',         LN27220
     4     ' 5.624710E-02-1.1713E-03 4.628520E-02-7.9024E-04-1'/         LN27230
      DATA CPL145/                                                       LN27240
     5     ' 22  717.193951 8.283E-25 9.823E-01.0607.0682 1809',         LN27250
     5     '.27430.75 0.00000  5  2             Q 54 422    -1',         LN27260
     5     ' 2 7.103980E-02-1.9092E-03 5.301270E-02-1.0579E-03',         LN27270
     5     ' 4.228770E-02-6.6755E-04 3.492840E-02-4.5020E-04-1',         LN27280
     6     ' 21  717.195543 3.963E-23 2.126E+00.0589.0643 2097',         LN27290
     6     '.54950.75 0.00000  5  2             Q 60 422    -1',         LN27300
     6     ' 2 9.685650E-02-3.5025E-03 7.219029E-02-1.9423E-03',         LN27310
     6     ' 5.740930E-02-1.2254E-03 4.719910E-02-8.2566E-04-1',         LN27320
     7     ' 21  717.411994 5.963E-23 2.036E+00.0595.0654 2004',         LN27330
     7     '.66020.75 0.00000  5  2             Q 58 422    -1',         LN27340
     7     ' 2 9.922510E-02-3.6889E-03 7.383480E-02-2.0397E-03',         LN27350
     7     ' 5.864430E-02-1.2839E-03 4.816890E-02-8.6360E-04-1',         LN27360
     8     ' 22  717.500123 1.198E-24 9.462E-01.0613.0699 1725',         LN27370
     8     '.71450.75 0.00000  5  2             Q 52 422    -1',         LN27380
     8     ' 2 7.318740E-02-2.0359E-03 5.445180E-02-1.1238E-03',         LN27390
     8     ' 4.331470E-02-7.0691E-04 3.568110E-02-4.7559E-04-1'/         LN27400
      DATA CPL149/                                                       LN27410
     9     ' 21  717.623636 8.830E-23 1.948E+00.0601.0667 1914',         LN27420
     9     '.87860.75 0.00000  5  2             Q 56 422    -1',         LN27430
     9     ' 2 1.017588E-01-3.8921E-03 7.558980E-02-2.1450E-03',         LN27440
     9     ' 5.995990E-02-1.3468E-03 4.919980E-02-9.0405E-04-1',         LN27450
     *     ' 22  717.796382 1.705E-24 9.102E-01.0619.0718 1645',         LN27460
     *     '.26680.75 0.00000  5  2             Q 50 422    -1',         LN27470
     *     ' 2 7.544030E-02-2.1740E-03 5.597410E-02-1.1947E-03',         LN27480
     *     ' 4.441710E-02-7.4884E-04 3.650790E-02-5.0233E-04-1',         LN27490
     1     ' 21  717.830204 1.287E-22 1.862E+00.0607.0682 1828',         LN27500
     1     '.20610.75 0.00000  5  2             Q 54 422    -1',         LN27510
     1     ' 2 1.043289E-01-4.1088E-03 7.735520E-02-2.2563E-03',         LN27520
     1     ' 6.127290E-02-1.4124E-03 5.022420E-02-9.4589E-04-1',         LN27530
     2     ' 21  718.031444 1.845E-22 1.778E+00.0613.0699 1744',         LN27540
     2     '.64440.75 0.00000  5  2             Q 52 422    -1',         LN27550
     2     ' 2 1.070121E-01-4.3477E-03 7.916740E-02-2.3776E-03',         LN27560
     2     ' 6.260280E-02-1.4834E-03 5.124340E-02-9.9060E-04-1'/         LN27570
      DATA CPL153/                                                       LN27580
     3     ' 22  718.082541 2.386E-24 8.741E-01.0625.0739 1567',         LN27590
     3     '.93230.75 0.00000  5  2             Q 48 422    -1',         LN27600
     3     ' 2 7.788170E-02-2.3286E-03 5.762640E-02-1.2733E-03',         LN27610
     3     ' 4.562220E-02-7.9487E-04 3.742050E-02-5.3140E-04-1',         LN27620
     4     ' 21  718.227115 2.602E-22 1.695E+00.0619.0718 1664',         LN27630
     4     '.19460.75 0.00000  5  2             Q 50 422    -1',         LN27640
     4     ' 2 1.102465E-01-4.6324E-03 8.138260E-02-2.5228E-03',         LN27650
     4     ' 6.424470E-02-1.5687E-03 5.251870E-02-1.0445E-03-1',         LN27660
     5     ' 22  718.358427 3.282E-24 8.381E-01.0631.0761 1493',         LN27670
     5     '.71250.75 0.00000  5  2             Q 46 422    -1',         LN27680
     5     ' 2 8.071569E-02-2.5067E-03 5.957510E-02-1.3636E-03',         LN27690
     5     ' 4.706650E-02-8.4744E-04 3.853850E-02-5.6436E-04-1',         LN27700
     6     ' 21  718.416984 3.611E-22 1.615E+00.0625.0739 1586',         LN27710
     6     '.85820.75 0.00000  5  2             Q 48 422    -1',         LN27720
     6     ' 2 1.136668E-01-4.9378E-03 8.373820E-02-2.6772E-03',         LN27730
     6     ' 6.600230E-02-1.6584E-03 5.389279E-02-1.1007E-03-1'/         LN27740
      DATA CPL157/                                                       LN27750
     7     ' 21  718.600831 4.928E-22 1.536E+00.0631.0761 1512',         LN27760
     7     '.63650.75 0.00000  5  2             Q 46 422    -1',         LN27770
     7     ' 2 1.170429E-01-5.2550E-03 8.601059E-02-2.8332E-03',         LN27780
     7     ' 6.766370E-02-1.7463E-03 5.516550E-02-1.1539E-03-1',         LN27790
     8     ' 22  718.623871 4.440E-24 8.020E-01.0637.0784 1422',         LN27800
     8     '.60850.75 0.00000  5  2             Q 44 422    -1',         LN27810
     8     ' 2 8.358610E-02-2.6898E-03 6.153550E-02-1.4537E-03',         LN27820
     8     ' 4.851600E-02-8.9822E-04 3.965780E-02-5.9505E-04-1',         LN27830
     9     ' 21  718.778445 6.614E-22 1.459E+00.0637.0784 1441',         LN27840
     9     '.53080.75 0.00000  5  2             Q 44 422    -1',         LN27850
     9     ' 2 1.210235E-01-5.6217E-03 8.872240E-02-3.0117E-03',         LN27860
     9     ' 6.966700E-02-1.8457E-03 5.671770E-02-1.2132E-03-1',         LN27870
     *     ' 22  718.878717 5.903E-24 7.660E-01.0643.0808 1354',         LN27880
     *     '.62160.75 0.00000  5  2             Q 42 422    -1',         LN27890
     *     ' 2 8.661120E-02-2.8773E-03 6.358039E-02-1.5414E-03',         LN27900
     *     ' 5.001230E-02-9.4474E-04 4.080440E-02-6.2109E-04-1'/         LN27910
      DATA CPL161/                                                       LN27920
     1     ' 21  718.949626 8.729E-22 1.383E+00.0643.0808 1373',         LN27930
     1     '.54210.75 0.00000  5  2             Q 42 422    -1',         LN27940
     1     ' 2 1.248195E-01-5.9652E-03 9.123790E-02-3.1668E-03',         LN27950
     1     ' 7.147790E-02-1.9244E-03 5.808530E-02-1.2545E-03-1',         LN27960
     2     ' 21  719.114183 1.132E-21 1.309E+00.0649.0833 1308',         LN27970
     2     '.67160.75 0.00000  5  2             Q 40 421    -1',         LN27980
     2     ' 2 1.294020E-01-6.2158E-03 9.431890E-02-3.2432E-03',         LN27990
     2     ' 7.372950E-02-1.9370E-03 5.981170E-02-1.2400E-03-1',         LN28000
     3     ' 22  719.122814 7.714E-24 7.299E-01.0649.0833 1289',         LN28010
     3     '.75280.75 0.00000  5  2             Q 40 421    -1',         LN28020
     3     ' 2 8.986770E-02-3.0069E-03 6.577610E-02-1.5840E-03',         LN28030
     3     ' 5.161910E-02-9.5497E-04 4.203550E-02-6.1724E-04-1',         LN28040
     4     ' 21  719.271936 1.444E-21 1.236E+00.0657.0859 1246',         LN28050
     4     '.92050.75 0.00000  5  2             Q 38 421    -1',         LN28060
     4     ' 2 1.341990E-01-6.9127E-03 9.751039E-02-3.6185E-03',         LN28070
     4     ' 7.604089E-02-2.1706E-03 6.156930E-02-1.3983E-03-1'/         LN28080
      DATA CPL165/                                                       LN28090
     5     ' 22  719.356021 9.903E-24 6.939E-01.0657.0859 1228',         LN28100
     5     '.00330.75 0.00000  5  2             Q 38 421    -1',         LN28110
     5     ' 2 9.332569E-02-3.3510E-03 6.808750E-02-1.7690E-03',         LN28120
     5     ' 5.329740E-02-1.0700E-03 4.331600E-02-6.9491E-04-1',         LN28130
     6     ' 21  719.422714 1.809E-21 1.164E+00.0664.0885 1188',         LN28140
     6     '.28970.75 0.00000  5  2             Q 36 421    -1',         LN28150
     6     ' 2 1.389830E-01-7.1973E-03 1.006525E-01-3.6884E-03',         LN28160
     6     ' 7.828210E-02-2.1650E-03 6.325150E-02-1.3627E-03-1',         LN28170
     7     ' 21  719.566357 2.227E-21 1.094E+00.0672.0910 1132',         LN28180
     7     '.78030.75 0.00000  5  2             Q 34 421    -1',         LN28190
     7     ' 2 1.440660E-01-7.6545E-03 1.039181E-01-3.8590E-03',         LN28200
     7     ' 8.057009E-02-2.2272E-03 6.493630E-02-1.3767E-03-1',         LN28210
     8     ' 22  719.578204 1.249E-23 6.578E-01.0664.0885 1169',         LN28220
     8     '.37410.75 0.00000  5  2             Q 36 421    -1',         LN28230
     8     ' 2 9.698519E-02-3.5144E-03 7.051460E-02-1.8178E-03',         LN28240
     8     ' 5.504720E-02-1.0770E-03 4.463940E-02-6.8451E-04-1'/         LN28250
      DATA CPL169/                                                       LN28260
     9     ' 21  719.702713 2.691E-21 1.024E+00.0681.0935 1080',         LN28270
     9     '.39310.75 0.00000  5  2             Q 32 421    -1',         LN28280
     9     ' 2 1.494610E-01-8.3243E-03 1.073228E-01-4.1488E-03',         LN28290
     9     ' 8.290230E-02-2.3673E-03 6.661460E-02-1.4468E-03-1',         LN28300
     *     ' 22  719.789238 1.547E-23 6.218E-01.0672.0910 1113',         LN28310
     *     '.86610.75 0.00000  5  2             Q 34 421    -1',         LN28320
     *     ' 2 1.005537E-01-3.7426E-03 7.280520E-02-1.9045E-03',         LN28330
     *     ' 5.664620E-02-1.1098E-03 4.581200E-02-6.9332E-04-1',         LN28340
     1     ' 21  719.831640 3.192E-21 9.563E-01.0690.0960 1031',         LN28350
     1     '.12910.75 0.00000  5  2             Q 30 421    -1',         LN28360
     1     ' 2 1.558050E-01-8.9053E-03 1.113892E-01-4.3364E-03',         LN28370
     1     ' 8.574410E-02-2.4121E-03 6.870370E-02-1.4322E-03-1',         LN28380
     2     ' 21  719.953003 3.715E-21 8.890E-01.0699.0983  984',         LN28390
     2     '.98900.75 0.00000  5  2             Q 28 421    -1',         LN28400
     2     ' 2 1.614730E-01-8.9419E-03 1.147835E-01-4.1240E-03',         LN28410
     2     ' 8.794499E-02-2.1473E-03 7.019740E-02-1.1704E-03-1'/         LN28420
      DATA CPL173/                                                       LN28430
     3     ' 22  719.989004 1.880E-23 5.857E-01.0681.0935 1061',         LN28440
     3     '.48030.75 0.00000  5  2             Q 32 421    -1',         LN28450
     3     ' 2 1.046682E-01-4.0936E-03 7.545330E-02-2.0601E-03',         LN28460
     3     ' 5.850000E-02-1.1875E-03 4.717700E-02-7.3389E-04-1',         LN28470
     4     ' 21  720.066680 4.238E-21 8.227E-01.0710.1005  941',         LN28480
     4     '.97360.75 0.00000  5  2             Q 26 421    -1',         LN28490
     4     ' 2 1.678820E-01-9.6879E-03 1.185977E-01-4.3486E-03',         LN28500
     4     ' 9.040070E-02-2.1901E-03 7.184710E-02-1.1418E-03-1',         LN28510
     5     ' 21  720.172556 4.736E-21 7.573E-01.0721.1026  902',         LN28520
     5     '.08370.75 0.00000  5  2             Q 24 421    -1',         LN28530
     5     ' 2 1.743560E-01-9.6145E-03 1.222780E-01-3.9500E-03',         LN28540
     5     ' 9.264190E-02-1.7423E-03 7.325239E-02-7.1746E-04-1',         LN28550
     6     ' 22  720.177391 2.242E-23 5.497E-01.0690.0960 1012',         LN28560
     6     '.21760.75 0.00000  5  2             Q 30 421    -1',         LN28570
     6     ' 2 1.090856E-01-4.3859E-03 7.827040E-02-2.1572E-03',         LN28580
     6     ' 6.045650E-02-1.2133E-03 4.860570E-02-7.2974E-04-1'/         LN28590
      DATA CPL177/                                                       LN28600
     7     ' 21  720.270522 5.180E-21 6.927E-01.0733.1045  865',         LN28610
     7     '.31990.75 0.00000  5  2             Q 22 421    -1',         LN28620
     7     ' 2 1.811810E-01-8.9716E-03 1.259908E-01-3.0953E-03',         LN28630
     7     ' 9.477130E-02-9.3032E-04 7.447830E-02 7.9618E-07-1',         LN28640
     8     ' 22  720.354298 2.623E-23 5.137E-01.0699.0983  966',         LN28650
     8     '.07870.75 0.00000  5  2             Q 28 421    -1',         LN28660
     8     ' 2 1.136772E-01-4.4613E-03 8.113560E-02-2.0879E-03',         LN28670
     8     ' 6.240389E-02-1.1070E-03 4.999670E-02-6.1834E-04-1',         LN28680
     9     ' 21  720.360483 5.538E-21 6.287E-01.0747.1063  831',         LN28690
     9     '.68290.75 0.00000  5  2             Q 20 421    -1',         LN28700
     9     ' 2 1.868100E-01-8.5502E-03 1.284634E-01-2.3335E-03',         LN28710
     9     ' 9.570209E-02-1.7355E-04 7.457450E-02 6.7579E-04-1',         LN28720
     *     ' 21  720.442349 5.778E-21 5.655E-01.0761.1078  801',         LN28730
     *     '.17310.75 0.00000  5  2             Q 18 421    -1',         LN28740
     *     ' 2 1.922960E-01-5.2611E-03 1.304680E-01 4.7493E-04',         LN28750
     *     ' 9.604270E-02 2.1339E-03 7.404929E-02 2.5707E-03-1'/         LN28760
      DATA CPL181/                                                       LN28770
     1     ' 21  720.516040 5.870E-21 5.028E-01.0777.1092  773',         LN28780
     1     '.79110.75 0.00000  5  2             Q 16 421    -1',         LN28790
     1     ' 2 1.957410E-01-1.0498E-03 1.303770E-01 3.9495E-03',         LN28800
     1     ' 9.436959E-02 4.9235E-03 7.163000E-02 4.8188E-03-1',         LN28810
     2     ' 22  720.519626 3.007E-23 4.776E-01.0710.1005  923',         LN28820
     2     '.06450.75 0.00000  5  2             Q 26 421    -1',         LN28830
     2     ' 2 1.183806E-01-4.8312E-03 8.399169E-02-2.2001E-03',         LN28840
     2     ' 6.428760E-02-1.1289E-03 5.130060E-02-6.0466E-04-1',         LN28850
     3     ' 21  720.581483 5.789E-21 4.407E-01.0793.1103  749',         LN28860
     3     '.53740.75 0.00000  5  2             Q 14 421    -1',         LN28870
     3     ' 2 1.939860E-01 1.0416E-02 1.255592E-01 1.2266E-02',         LN28880
     3     ' 8.840390E-02 1.1224E-02 6.531590E-02 9.7422E-03-1',         LN28890
     4     ' 21  720.638616 5.518E-21 3.790E-01.0810.1113  728',         LN28900
     4     '.41230.75 0.00000  5  2             Q 12 421    -1',         LN28910
     4     ' 2 1.870180E-01 3.1290E-02 1.158144E-01 2.6628E-02',         LN28920
     4     ' 7.788300E-02 2.1772E-02 5.482620E-02 1.7819E-02-1'/         LN28930
      DATA CPL185/                                                       LN28940
     5     ' 22  720.673288 3.375E-23 4.416E-01.0721.1026  883',         LN28950
     5     '.17570.75 0.00000  5  2             Q 24 421    -1',         LN28960
     5     ' 2 1.227538E-01-4.7939E-03 8.643050E-02-2.0046E-03',         LN28970
     5     ' 6.573319E-02-9.0973E-04 5.217160E-02-3.9702E-04-1',         LN28980
     6     ' 21  720.687382 5.049E-21 3.177E-01.0826.1121  710',         LN28990
     6     '.41630.75 0.00000  5  2             Q 10 421    -1',         LN29000
     6     ' 2 1.585090E-01 8.7192E-02 8.787480E-02 6.3050E-02',         LN29010
     6     ' 5.152030E-02 4.7761E-02 3.030950E-02 3.7406E-02-1',         LN29020
     7     ' 21  720.727734 4.386E-21 2.568E-01.0843.1129  695',         LN29030
     7     '.54960.75 0.00000  5  2             Q  8 421    -1',         LN29040
     7     ' 2 9.608299E-02 2.1607E-01 3.160300E-02 1.4365E-01',         LN29050
     7     ' 6.649499E-04 1.0379E-01-1.578460E-02 7.8936E-02-1',         LN29060
     8     ' 21  720.759635 3.547E-21 1.961E-01.0860.1136  683',         LN29070
     8     '.81240.75 0.00000  5  2             Q  6 421    -1',         LN29080
     8     ' 2-7.450300E-02 5.8986E-01-1.121445E-01 3.6877E-01',         LN29090
     8     '-1.243398E-01 2.5652E-01-1.264484E-01 1.9046E-01-1'/         LN29100
      DATA CPL189/                                                       LN29110
     9     ' 21  720.783053 2.558E-21 1.356E-01.0876.1146  675',         LN29120
     9     '.20500.75 0.00000  5  2             Q  4 421    -1',         LN29130
     9     ' 2-6.337760E-01 1.9115E+00-5.628870E-01 1.1329E+00',         LN29140
     9     '-5.056220E-01 7.6077E-01-4.582500E-01 5.5268E-01-1',         LN29150
     *     ' 21  720.797966 1.459E-21 7.530E-02.0892.1173  669',         LN29160
     *     '.72740.75 0.00000  5  2             Q  2 421    -1',         LN29170
     *     ' 2-4.101890E+00-5.5910E+00-3.252210E+00-3.5814E+00',         LN29180
     *     '-2.723500E+00-2.5454E+00-2.359110E+00-1.9280E+00-1',         LN29190
     1     ' 22  720.815202 3.707E-23 4.055E-01.0733.1045  846',         LN29200
     1     '.41290.75 0.00000  5  2             Q 22 421    -1',         LN29210
     1     ' 2 1.277770E-01-4.5231E-03 8.922550E-02-1.6129E-03',         LN29220
     1     ' 6.738810E-02-5.3026E-04 5.317260E-02-5.8263E-05-1',         LN29230
     2     ' 22  720.945292 3.978E-23 3.695E-01.0747.1063  812',         LN29240
     2     '.77680.75 0.00000  5  2             Q 20 421    -1',         LN29250
     2     ' 2 1.319890E-01-4.3334E-03 9.115729E-02-1.2470E-03',         LN29260
     2     ' 6.820190E-02-1.6272E-04 5.337540E-02 2.7130E-04-1'/         LN29270
      DATA CPL193/                                                       LN29280
     3     ' 22  721.063490 4.165E-23 3.334E-01.0761.1078  782',         LN29290
     3     '.26780.75 0.00000  5  2             Q 18 421    -1',         LN29300
     3     ' 2 1.356030E-01-2.7440E-03 9.238839E-02 1.1248E-04',         LN29310
     3     ' 6.829810E-02 9.5528E-04 5.288530E-02 1.1899E-03-1',         LN29320
     4     ' 22  721.169734 4.244E-23 2.974E-01.0777.1092  754',         LN29330
     4     '.88660.75 0.00000  5  2             Q 16 421    -1',         LN29340
     4     ' 2 1.386190E-01-7.6398E-04 9.278880E-02 1.7699E-03',         LN29350
     4     ' 6.750510E-02 2.2948E-03 5.151250E-02 2.2737E-03-1',         LN29360
     5     ' 22  721.263970 4.197E-23 2.613E-01.0793.1103  730',         LN29370
     5     '.63360.75 0.00000  5  2             Q 14 421    -1',         LN29380
     5     ' 2 1.378780E-01 4.8391E-03 8.982220E-02 5.8427E-03',         LN29390
     5     ' 6.367660E-02 5.3847E-03 4.739800E-02 4.6910E-03-1',         LN29400
     6     ' 22  721.346147 4.010E-23 2.253E-01.0810.1113  709',         LN29410
     6     '.50910.75 0.00000  5  2             Q 12 421    -1',         LN29420
     6     ' 2 1.319890E-01 1.5053E-02 8.217950E-02 1.2858E-02',         LN29430
     6     ' 5.561530E-02 1.0533E-02 3.944590E-02 8.6300E-03-1'/         LN29440
      DATA CPL197/                                                       LN29450
     7     ' 22  721.416225 3.676E-23 1.892E-01.0826.1121  691',         LN29460
     7     '.51350.75 0.00000  5  2             Q 10 421    -1',         LN29470
     7     ' 2 1.136798E-01 4.2236E-02 6.395220E-02 3.0605E-02',         LN29480
     7     ' 3.828890E-02 2.3214E-02 2.327000E-02 1.8199E-02-1',         LN29490
     8     ' 22  721.474167 3.199E-23 1.532E-01.0843.1129  676',         LN29500
     8     '.64720.75 0.00000  5  2             Q  8 421    -1',         LN29510
     8     ' 2 6.788730E-02 1.0503E-01 2.289690E-02 6.9849E-02',         LN29520
     8     ' 1.291992E-03 5.0482E-02-1.020968E-02 3.8401E-02-1',         LN29530
     9     ' 22  721.519944 2.590E-23 1.171E-01.0860.1136  664',         LN29540
     9     '.91040.75 0.00000  5  2             Q  6 421    -1',         LN29550
     9     ' 2-4.760730E-02 2.8782E-01-7.463690E-02 1.8005E-01',         LN29560
     9     '-8.363289E-02 1.2531E-01-8.544900E-02 9.3084E-02-1',         LN29570
     *     ' 22  721.553532 1.870E-23 8.110E-02.0876.1146  656',         LN29580
     *     '.30320.75 0.00000  5  2             Q  4 421    -1',         LN29590
     *     ' 2-4.419350E-01 9.2862E-01-3.923530E-01 5.5039E-01',         LN29600
     *     '-3.523260E-01 3.6960E-01-3.192540E-01 2.6851E-01-1'/         LN29610
      DATA CPL201/                                                       LN29620
     1     ' 22  721.574914 1.067E-23 4.506E-02.0892.1173  650',         LN29630
     1     '.82580.75 0.00000  5  2             Q  2 421    -1',         LN29640
     1     ' 2-2.852330E+00-2.7054E+00-2.261350E+00-1.7328E+00',         LN29650
     1     '-1.893580E+00-1.2315E+00-1.640210E+00-9.3270E-01-1',         LN29660
     2     ' 21  735.944714 1.127E-25 1.390E+00.0562.0618 3223',         LN29670
     2     '.66220.75 0.00000  8  4             Q 69 422    -1',         LN29680
     2     ' 2 9.022520E-02-3.0728E-03 7.330310E-02-2.0419E-03',         LN29690
     2     ' 6.255080E-02-1.4957E-03 5.468190E-02-1.1486E-03-1',         LN29700
     3     ' 21  736.256502 1.814E-25 1.329E+00.0568.0621 3116',         LN29710
     3     '.70050.75 0.00000  8  4             Q 67 422    -1',         LN29720
     3     ' 2 7.312370E-02-1.8191E-03 5.683080E-02-1.0497E-03',         LN29730
     3     ' 4.699109E-02-6.8853E-04 4.008160E-02-4.8173E-04-1',         LN29740
     4     ' 21  736.561084 2.873E-25 1.270E+00.0574.0625 3012',         LN29750
     4     '.84190.75 0.00000  8  4             Q 65 422    -1',         LN29760
     4     ' 2 7.174440E-02-1.8531E-03 5.457140E-02-1.0399E-03',         LN29770
     4     ' 4.430920E-02-6.6290E-04 3.720340E-02-4.5068E-04-1'/         LN29780
      DATA CPL205/                                                       LN29790
     5     ' 21  736.858282 4.480E-25 1.213E+00.0580.0631 2912',         LN29800
     5     '.08820.75 0.00000  8  4             Q 63 422    -1',         LN29810
     5     ' 2 7.287019E-02-1.9608E-03 5.487170E-02-1.0940E-03',         LN29820
     5     ' 4.413110E-02-6.9325E-04 3.672370E-02-4.6849E-04-1',         LN29830
     6     ' 21  737.147927 6.876E-25 1.158E+00.0586.0639 2814',         LN29840
     6     '.44120.75 0.00000  8  4             Q 61 422    -1',         LN29850
     6     ' 2 7.470319E-02-2.0838E-03 5.594940E-02-1.1592E-03',         LN29860
     6     ' 4.475510E-02-7.3287E-04 3.704740E-02-4.9433E-04-1',         LN29870
     7     ' 21  737.429854 1.039E-24 1.104E+00.0592.0649 2719',         LN29880
     7     '.90240.75 0.00000  8  4             Q 59 422    -1',         LN29890
     7     ' 2 7.679230E-02-2.2156E-03 5.731310E-02-1.2290E-03',         LN29900
     7     ' 4.568850E-02-7.7538E-04 3.768830E-02-5.2225E-04-1',         LN29910
     8     ' 21  737.703907 1.545E-24 1.053E+00.0598.0661 2628',         LN29920
     8     '.47340.75 0.00000  8  4             Q 57 422    -1',         LN29930
     8     ' 2 7.896589E-02-2.3581E-03 5.877950E-02-1.3042E-03',         LN29940
     8     ' 4.673890E-02-8.2098E-04 3.846180E-02-5.5212E-04-1'/         LN29950
      DATA CPL209/                                                       LN29960
     9     ' 21  737.969934 2.261E-24 1.002E+00.0604.0675 2540',         LN29970
     9     '.15600.75 0.00000  8  4             Q 55 422    -1',         LN29980
     9     ' 2 8.156589E-02-2.5251E-03 6.056960E-02-1.3916E-03',         LN29990
     9     ' 4.805970E-02-8.7381E-04 3.946930E-02-5.8652E-04-1',         LN30000
     *     ' 21  738.227789 3.256E-24 9.536E-01.0610.0691 2454',         LN30010
     *     '.95140.75 0.00000  8  4             Q 53 422    -1',         LN30020
     *     ' 2 8.414769E-02-2.6997E-03 6.235450E-02-1.4826E-03',         LN30030
     *     ' 4.938700E-02-9.2832E-04 4.049630E-02-6.2176E-04-1',         LN30040
     1     ' 21  738.477334 4.613E-24 9.064E-01.0616.0709 2372',         LN30050
     1     '.86120.75 0.00000  8  4             Q 51 422    -1',         LN30060
     1     ' 2 8.705840E-02-2.8991E-03 6.437080E-02-1.5856E-03',         LN30070
     1     ' 5.089240E-02-9.8948E-04 4.166760E-02-6.6092E-04-1',         LN30080
     2     ' 21  738.718436 6.432E-24 8.605E-01.0622.0728 2293',         LN30090
     2     '.88680.75 0.00000  8  4             Q 49 422    -1',         LN30100
     2     ' 2 9.002500E-02-3.1102E-03 6.641570E-02-1.6932E-03',         LN30110
     2     ' 5.241600E-02-1.0525E-03 4.285060E-02-7.0073E-04-1'/         LN30120
      DATA CPL213/                                                       LN30130
     3     ' 21  738.950967 8.821E-24 8.160E-01.0628.0750 2218',         LN30140
     3     '.02940.75 0.00000  8  4             Q 47 422    -1',         LN30150
     3     ' 2 9.320220E-02-3.3429E-03 6.859840E-02-1.8104E-03',         LN30160
     3     ' 5.403710E-02-1.1203E-03 4.411029E-02-7.4292E-04-1',         LN30170
     4     ' 21  739.174805 1.190E-23 7.727E-01.0634.0772 2145',         LN30180
     4     '.29050.75 0.00000  8  4             Q 45 422    -1',         LN30190
     4     ' 2 9.656010E-02-3.5928E-03 7.089030E-02-1.9335E-03',         LN30200
     4     ' 5.573229E-02-1.1898E-03 4.542200E-02-7.8503E-04-1',         LN30210
     5     ' 21  739.389835 1.579E-23 7.306E-01.0640.0796 2075',         LN30220
     5     '.67110.75 0.00000  8  4             Q 43 422    -1',         LN30230
     5     ' 2 1.000467E-01-3.8513E-03 7.324850E-02-2.0561E-03',         LN30240
     5     ' 5.746260E-02-1.2560E-03 4.675060E-02-8.2291E-04-1',         LN30250
     6     ' 21  739.595947 2.059E-23 6.896E-01.0646.0821 2009',         LN30260
     6     '.17250.75 0.00000  8  4             Q 41 422    -1',         LN30270
     6     ' 2 1.038362E-01-4.0735E-03 7.580040E-02-2.1455E-03',         LN30280
     6     ' 5.932679E-02-1.2934E-03 4.817929E-02-8.3608E-04-1'/         LN30290
      DATA CPL217/                                                       LN30300
     7     ' 21  739.793034 2.641E-23 6.497E-01.0653.0846 1945',         LN30310
     7     '.79590.75 0.00000  8  4             Q 39 421    -1',         LN30320
     7     ' 2 1.078103E-01-4.3689E-03 7.844719E-02-2.2786E-03',         LN30330
     7     ' 6.124170E-02-1.3608E-03 4.963140E-02-8.7161E-04-1',         LN30340
     8     ' 21  739.980998 3.329E-23 6.108E-01.0661.0872 1885',         LN30350
     8     '.54230.75 0.00000  8  4             Q 37 421    -1',         LN30360
     8     ' 2 1.120626E-01-4.8374E-03 8.127079E-02-2.5187E-03',         LN30370
     8     ' 6.328010E-02-1.5031E-03 5.117580E-02-9.6325E-04-1',         LN30380
     9     ' 21  740.159745 4.122E-23 5.728E-01.0668.0897 1828',         LN30390
     9     '.41260.75 0.00000  8  4             Q 35 421    -1',         LN30400
     9     ' 2 1.164813E-01-4.9880E-03 8.414640E-02-2.5233E-03',         LN30410
     9     ' 6.531460E-02-1.4609E-03 5.268770E-02-9.0557E-04-1',         LN30420
     *     ' 21  740.329185 5.014E-23 5.357E-01.0677.0923 1774',         LN30430
     *     '.40800.75 0.00000  8  4             Q 33 421    -1',         LN30440
     *     ' 2 1.214226E-01-5.5925E-03 8.737169E-02-2.8259E-03',         LN30450
     *     ' 6.760650E-02-1.6362E-03 5.439980E-02-1.0164E-03-1'/         LN30460
      DATA CPL221/                                                       LN30470
     1     ' 21  740.489236 5.987E-23 4.995E-01.0685.0947 1723',         LN30480
     1     '.52920.75 0.00000  8  4             Q 31 421    -1',         LN30490
     1     ' 2 1.261923E-01-5.6775E-03 9.037080E-02-2.7504E-03',         LN30500
     1     ' 6.965660E-02-1.5188E-03 5.587140E-02-8.9220E-04-1',         LN30510
     2     ' 21  740.564396 1.443E-25 1.359E+00.0565.0619 3169',         LN30520
     2     '.77080.75 0.00000  8  4             Q 68 422    -1',         LN30530
     2     ' 2 6.196060E-01-1.4033E-01 5.030480E-01-9.3046E-02',         LN30540
     2     ' 4.289610E-01-6.8019E-02 3.748290E-01-5.2164E-02-1',         LN30550
     3     ' 21  740.607203 2.302E-25 1.300E+00.0571.0623 3064',         LN30560
     3     '.36470.75 0.00000  8  4             Q 66 422    -1',         LN30570
     3     ' 2 4.570930E-01-6.4044E-02 3.525470E-01-3.4976E-02',         LN30580
     3     ' 2.898220E-01-2.1792E-02 2.460770E-01-1.4501E-02-1',         LN30590
     4     ' 21  740.639819 7.017E-23 4.640E-01.0695.0971 1675',         LN30600
     4     '.77720.75 0.00000  8  4             Q 29 421    -1',         LN30610
     4     ' 2 1.312610E-01-6.2297E-03 9.350250E-02-2.9748E-03',         LN30620
     4     ' 7.175609E-02-1.6179E-03 5.734820E-02-9.3471E-04-1'/         LN30630
      DATA CPL225/                                                       LN30640
     5     ' 21  740.651771 3.616E-25 1.241E+00.0577.0628 2962',         LN30650
     5     '.06200.75 0.00000  8  4             Q 64 422    -1',         LN30660
     5     ' 2 4.212650E-01-5.9257E-02 3.165500E-01-3.1416E-02',         LN30670
     5     ' 2.544880E-01-1.8920E-02 2.118740E-01-1.2125E-02-1',         LN30680
     6     ' 21  740.697785 5.592E-25 1.185E+00.0583.0635 2862',         LN30690
     6     '.86440.75 0.00000  8  4             Q 62 422    -1',         LN30700
     6     ' 2 4.096170E-01-5.8493E-02 3.043300E-01-3.1112E-02',         LN30710
     6     ' 2.419560E-01-1.8803E-02 1.993160E-01-1.2100E-02-1',         LN30720
     7     ' 21  740.744946 8.512E-25 1.131E+00.0589.0643 2766',         LN30730
     7     '.77380.75 0.00000  8  4             Q 60 422    -1',         LN30740
     7     ' 2 4.045990E-01-5.8140E-02 2.990130E-01-3.1027E-02',         LN30750
     7     ' 2.363920E-01-1.8837E-02 1.936090E-01-1.2195E-02-1',         LN30760
     8     ' 21  740.780860 8.065E-23 4.292E-01.0705.0994 1631',         LN30770
     8     '.15280.75 0.00000  8  4             Q 27 421    -1',         LN30780
     8     ' 2 1.368510E-01-6.5692E-03 9.691110E-02-3.0143E-03',         LN30790
     8     ' 7.401419E-02-1.5617E-03 5.891730E-02-8.4747E-04-1'/         LN30800
      DATA CPL229/                                                       LN30810
     9     ' 21  740.792971 1.275E-24 1.078E+00.0595.0654 2673',         LN30820
     9     '.79180.75 0.00000  8  4             Q 58 422    -1',         LN30830
     9     ' 2 4.009330E-01-5.7845E-02 2.953860E-01-3.0900E-02',         LN30840
     9     ' 2.327520E-01-1.8803E-02 1.899560E-01-1.2216E-02-1',         LN30850
     *     ' 21  740.841591 1.881E-24 1.027E+00.0601.0667 2583',         LN30860
     *     '.92020.75 0.00000  8  4             Q 56 422    -1',         LN30870
     *     ' 2 4.000620E-01-5.8157E-02 2.939950E-01-3.1054E-02',         LN30880
     *     ' 2.310750E-01-1.8914E-02 1.881230E-01-1.2312E-02-1',         LN30890
     1     ' 21  740.890550 2.730E-24 9.778E-01.0607.0682 2497',         LN30900
     1     '.16030.75 0.00000  8  4             Q 54 422    -1',         LN30910
     1     ' 2 4.015180E-01-5.9059E-02 2.946580E-01-3.1538E-02',         LN30920
     1     ' 2.312960E-01-1.9226E-02 1.880580E-01-1.2539E-02-1',         LN30930
     2     ' 21  740.912291 9.086E-23 3.951E-01.0715.1016 1589',         LN30940
     2     '.65670.75 0.00000  8  4             Q 25 421    -1',         LN30950
     2     ' 2 1.419340E-01-6.3337E-03 9.978019E-02-2.6316E-03',         LN30960
     2     ' 7.574320E-02-1.1795E-03 5.998070E-02-5.0011E-04-1'/         LN30970
      DATA CPL233/                                                       LN30980
     3     ' 21  740.939608 3.898E-24 9.298E-01.0613.0699 2413',         LN30990
     3     '.51390.75 0.00000  8  4             Q 52 422    -1',         LN31000
     3     ' 2 4.044560E-01-6.0316E-02 2.964130E-01-3.2161E-02',         LN31010
     3     ' 2.324010E-01-1.9591E-02 1.887470E-01-1.2777E-02-1',         LN31020
     4     ' 21  740.988538 5.478E-24 8.833E-01.0619.0718 2332',         LN31030
     4     '.98230.75 0.00000  8  4             Q 50 422    -1',         LN31040
     4     ' 2 4.081740E-01-6.1781E-02 2.986100E-01-3.2844E-02',         LN31050
     4     ' 2.337920E-01-1.9963E-02 1.896570E-01-1.2998E-02-1',         LN31060
     5     ' 21  741.034049 1.002E-22 3.617E-01.0727.1036 1551',         LN31070
     5     '.28960.75 0.00000  8  4             Q 23 421    -1',         LN31080
     5     ' 2 1.477450E-01-6.3688E-03 1.030185E-01-2.3889E-03',         LN31090
     5     ' 7.766330E-02-8.8566E-04 6.114160E-02-2.1625E-04-1',         LN31100
     6     ' 21  741.037124 7.573E-24 8.381E-01.0625.0739 2255',         LN31110
     6     '.56690.75 0.00000  8  4             Q 48 422    -1',         LN31120
     6     ' 2 4.127890E-01-6.3565E-02 3.013790E-01-3.3673E-02',         LN31130
     6     ' 2.355860E-01-2.0407E-02 1.908660E-01-1.3257E-02-1'/         LN31140
      DATA CPL237/                                                       LN31150
     7     ' 21  741.085163 1.030E-23 7.942E-01.0631.0761 2181',         LN31160
     7     '.26910.75 0.00000  8  4             Q 46 422    -1',         LN31170
     7     ' 2 4.196530E-01-6.5924E-02 3.058770E-01-3.4785E-02',         LN31180
     7     ' 2.387970E-01-2.1011E-02 1.932840E-01-1.3608E-02-1',         LN31190
     8     ' 21  741.132466 1.377E-23 7.515E-01.0637.0784 2110',         LN31200
     8     '.09020.75 0.00000  8  4             Q 44 422    -1',         LN31210
     8     ' 2 4.255680E-01-6.8056E-02 3.094130E-01-3.5681E-02',         LN31220
     8     ' 2.410980E-01-2.1424E-02 1.948440E-01-1.3799E-02-1',         LN31230
     9     ' 21  741.146076 1.081E-22 3.287E-01.0740.1054 1516',         LN31240
     9     '.05210.75 0.00000  8  4             Q 21 421    -1',         LN31250
     9     ' 2 1.527890E-01-5.7918E-03 1.054547E-01-1.6892E-03',         LN31260
     9     ' 7.880340E-02-2.3972E-04 6.156410E-02 3.4557E-04-1',         LN31270
     *     ' 21  741.178852 1.811E-23 7.099E-01.0643.0808 2042',         LN31280
     *     '.03140.75 0.00000  8  4             Q 42 422    -1',         LN31290
     *     ' 2 4.339270E-01-7.0730E-02 3.146650E-01-3.6796E-02',         LN31300
     *     ' 2.446990E-01-2.1931E-02 1.974310E-01-1.4022E-02-1'/         LN31310
      DATA CPL241/                                                       LN31320
     1     ' 21  741.224152 2.342E-23 6.695E-01.0649.0833 1977',         LN31330
     1     '.09380.75 0.00000  8  4             Q 40 421    -1',         LN31340
     1     ' 2 4.461210E-01-7.2097E-02 3.229070E-01-3.6841E-02',         LN31350
     1     ' 2.507570E-01-2.1549E-02 2.021370E-01-1.3497E-02-1',         LN31360
     2     ' 21  741.248318 1.139E-22 2.963E-01.0754.1071 1483',         LN31370
     2     '.94500.75 0.00000  8  4             Q 19 421    -1',         LN31380
     2     ' 2 1.571440E-01-4.3365E-03 1.070810E-01-3.4252E-04',         LN31390
     2     ' 7.911800E-02 9.0000E-04 6.118710E-02 1.2946E-03-1',         LN31400
     3     ' 21  741.268208 2.978E-23 6.301E-01.0657.0859 1915',         LN31410
     3     '.27870.75 0.00000  8  4             Q 38 421    -1',         LN31420
     3     ' 2 4.545970E-01-7.7984E-02 3.278600E-01-4.0030E-02',         LN31430
     3     ' 2.538770E-01-2.3560E-02 2.041650E-01-1.4886E-02-1',         LN31440
     4     ' 21  741.310873 3.719E-23 5.917E-01.0664.0885 1856',         LN31450
     4     '.58700.75 0.00000  8  4             Q 36 421    -1',         LN31460
     4     ' 2 4.675320E-01-7.9943E-02 3.361670E-01-4.0136E-02',         LN31470
     4     ' 2.597010E-01-2.3071E-02 2.084680E-01-1.4200E-02-1'/         LN31480
      DATA CPL245/                                                       LN31490
     5     ' 21  741.340727 1.170E-22 2.643E-01.0769.1085 1454',         LN31500
     5     '.96860.75 0.00000  8  4             Q 17 421    -1',         LN31510
     5     ' 2 1.596400E-01-1.1603E-03 1.068340E-01 2.1979E-03',         LN31520
     5     ' 7.764380E-02 2.9204E-03 5.913960E-02 2.9193E-03-1',         LN31530
     6     ' 21  741.352008 4.564E-23 5.541E-01.0672.0910 1801',         LN31540
     6     '.01980.75 0.00000  8  4             Q 34 421    -1',         LN31550
     6     ' 2 4.798430E-01-8.3291E-02 3.436290E-01-4.1105E-02',         LN31560
     6     ' 2.646020E-01-2.3200E-02 2.118480E-01-1.3991E-02-1',         LN31570
     7     ' 21  741.391483 5.500E-23 5.175E-01.0681.0935 1748',         LN31580
     7     '.57800.75 0.00000  8  4             Q 32 421    -1',         LN31590
     7     ' 2 4.948060E-01-8.9475E-02 3.529890E-01-4.3724E-02',         LN31600
     7     ' 2.709850E-01-2.4432E-02 2.164370E-01-1.4585E-02-1',         LN31610
     8     ' 21  741.423258 1.168E-22 2.327E-01.0785.1098 1429',         LN31620
     8     '.12360.75 0.00000  8  4             Q 15 421    -1',         LN31630
     8     ' 2 1.579370E-01 4.8221E-03 1.028391E-01 6.5901E-03',         LN31640
     8     ' 7.280130E-02 6.2633E-03 5.404880E-02 5.5350E-03-1'/         LN31650
      DATA CPL249/                                                       LN31660
     9     ' 21  741.429180 6.506E-23 4.816E-01.0690.0960 1699',         LN31670
     9     '.26250.75 0.00000  8  4             Q 30 421    -1',         LN31680
     9     ' 2 5.094830E-01-9.3395E-02 3.616990E-01-4.4444E-02',         LN31690
     9     ' 2.765750E-01-2.4094E-02 2.201680E-01-1.3875E-02-1',         LN31700
     *     ' 21  741.464987 7.550E-23 4.465E-01.0699.0983 1653',         LN31710
     *     '.07420.75 0.00000  8  4             Q 28 421    -1',         LN31720
     *     ' 2 5.241340E-01-9.2297E-02 3.699150E-01-4.1397E-02',         LN31730
     *     ' 2.814760E-01-2.0804E-02 2.231580E-01-1.0783E-02-1',         LN31740
     1     ' 21  741.495874 1.129E-22 2.013E-01.0801.1108 1406',         LN31750
     1     '.41030.75 0.00000  8  4             Q 13 421    -1',         LN31760
     1     ' 2 1.512420E-01 1.8996E-02 9.427210E-02 1.6325E-02',         LN31770
     1     ' 6.379230E-02 1.3426E-02 4.516720E-02 1.1037E-02-1',         LN31780
     2     ' 21  741.498801 8.591E-23 4.121E-01.0710.1005 1610',         LN31790
     2     '.01380.75 0.00000  8  4             Q 26 421    -1',         LN31800
     2     ' 2 5.449990E-01-9.9716E-02 3.826550E-01-4.3577E-02',         LN31810
     2     ' 2.899520E-01-2.1169E-02 2.290990E-01-1.0446E-02-1'/         LN31820
      DATA CPL253/                                                       LN31830
     3     ' 21  741.530529 9.576E-23 3.783E-01.0721.1026 1570',         LN31840
     3     '.08210.75 0.00000  8  4             Q 24 421    -1',         LN31850
     3     ' 2 5.596890E-01-9.5359E-02 3.897140E-01-3.7356E-02',         LN31860
     3     ' 2.932280E-01-1.5161E-02 2.302560E-01-5.0940E-03-1',         LN31870
     4     ' 21  741.558540 1.050E-22 1.702E-01.0818.1117 1386',         LN31880
     4     '.82910.75 0.00000  8  4             Q 11 421    -1',         LN31890
     4     ' 2 1.279564E-01 4.7939E-02 7.173140E-02 3.5139E-02',         LN31900
     4     ' 4.264130E-02 2.6809E-02 2.554240E-02 2.1086E-02-1',         LN31910
     5     ' 21  741.560082 1.045E-22 3.451E-01.0733.1045 1533',         LN31920
     5     '.27970.75 0.00000  8  4             Q 22 421    -1',         LN31930
     5     ' 2 5.755100E-01-8.6311E-02 3.971760E-01-2.7262E-02',         LN31940
     5     ' 2.965560E-01-6.0146E-03 2.313350E-01 2.8170E-03-1',         LN31950
     6     ' 21  741.587384 1.114E-22 3.124E-01.0747.1063 1499',         LN31960
     6     '.60730.75 0.00000  8  4             Q 20 421    -1',         LN31970
     6     ' 2 5.945030E-01-7.9882E-02 4.056260E-01-1.7976E-02',         LN31980
     6     ' 2.998580E-01 2.8337E-03 2.318420E-01 1.0573E-02-1'/         LN31990
      DATA CPL257/                                                       LN32000
     7     ' 21  741.611226 9.304E-23 1.392E-01.0835.1125 1370',         LN32010
     7     '.38040.75 0.00000  8  4             Q  9 421    -1',         LN32020
     7     ' 2 7.440159E-02 1.1693E-01 2.420210E-02 7.8256E-02',         LN32030
     7     ' 2.723240E-05 5.6749E-02-1.292473E-02 4.3246E-02-1',         LN32040
     8     ' 21  741.612361 1.159E-22 2.802E-01.0761.1078 1469',         LN32050
     8     '.06550.75 0.00000  8  4             Q 18 421    -1',         LN32060
     8     ' 2 5.982470E-01-3.8120E-02 4.012710E-01 1.5179E-02',         LN32070
     8     ' 2.920450E-01 2.9179E-02 2.225470E-01 3.1781E-02-1',         LN32080
     9     ' 21  741.634950 1.174E-22 2.484E-01.0777.1092 1441',         LN32090
     9     '.65470.75 0.00000  8  4             Q 16 421    -1',         LN32100
     9     ' 2 6.028100E-01 6.7136E-03 3.958890E-01 5.1272E-02',         LN32110
     9     ' 2.824380E-01 5.7804E-02 2.111460E-01 5.4666E-02-1',         LN32120
     *     ' 21  741.653908 7.711E-23 1.082E-01.0852.1132 1357',         LN32130
     *     '.06440.75 0.00000  8  4             Q  7 421    -1',         LN32140
     *     ' 2-6.491030E-02 2.9518E-01-9.238189E-02 1.8525E-01',         LN32150
     *     '-1.009684E-01 1.2909E-01-1.021657E-01 9.5889E-02-1'/         LN32160
      DATA CPL261/                                                       LN32170
     1     ' 21  741.655094 1.154E-22 2.170E-01.0793.1103 1417',         LN32180
     1     '.37550.75 0.00000  8  4             Q 14 421    -1',         LN32190
     1     ' 2 6.053840E-01 1.3380E-01 3.871400E-01 1.4271E-01',         LN32200
     1     ' 2.690220E-01 1.2683E-01 1.959230E-01 1.0852E-01-1',         LN32210
     2     ' 21  741.672743 1.095E-22 1.858E-01.0810.1113 1396',         LN32220
     2     '.22820.75 0.00000  8  4             Q 12 421    -1',         LN32230
     2     ' 2 5.354050E-01 3.6899E-01 3.185520E-01 2.9945E-01',         LN32240
     2     ' 2.042820E-01 2.3969E-01 1.356290E-01 1.9374E-01-1',         LN32250
     3     ' 21  741.686567 5.742E-23 7.664E-02.0868.1140 1346',         LN32260
     3     '.88140.75 0.00000  8  4             Q  5 421    -1',         LN32270
     3     ' 2-4.931420E-01 7.9608E-01-4.370860E-01 4.7161E-01',         LN32280
     3     '-3.923790E-01 3.1615E-01-3.557580E-01 2.2901E-01-1',         LN32290
     4     ' 21  741.687854 9.954E-23 1.547E-01.0826.1121 1378',         LN32300
     4     '.21320.75 0.00000  8  4             Q 10 421    -1',         LN32310
     4     ' 2 4.498130E-01 9.6794E-01 2.335320E-01 6.8916E-01',         LN32320
     4     ' 1.235390E-01 5.1762E-01 6.027710E-02 4.0323E-01-1'/         LN32330
      DATA CPL265/                                                       LN32340
     5     ' 21  741.700389 8.557E-23 1.237E-01.0843.1129 1363',         LN32350
     5     '.33080.75 0.00000  8  4             Q  8 421    -1',         LN32360
     5     ' 2 1.593280E-01 2.3809E+00-1.838850E-02 1.5630E+00',         LN32370
     5     '-9.934209E-02 1.1203E+00-1.392430E-01 8.4738E-01-1',         LN32380
     6     ' 21  741.709186 3.372E-23 4.349E-02.0884.1155 1339',         LN32390
     6     '.83150.75 0.00000  8  4             Q  3 421    -1',         LN32400
     6     ' 2-2.662660E+00-2.4121E+00-2.129400E+00-1.5701E+00',         LN32410
     6     '-1.793870E+00-1.1281E+00-1.560000E+00-8.6066E-01-1',         LN32420
     7     ' 21  741.710319 6.773E-23 9.249E-02.0860.1136 1351',         LN32430
     7     '.58130.75 0.00000  8  4             Q  6 421    -1',         LN32440
     7     ' 2-5.914870E-01 6.1892E+00-6.393920E-01 3.8290E+00',         LN32450
     7     '-6.333990E-01 2.6437E+00-6.089720E-01 1.9523E+00-1',         LN32460
     8     ' 21  741.717619 4.614E-23 6.042E-02.0876.1146 1342',         LN32470
     8     '.96480.75 0.00000  8  4             Q  4 421    -1',         LN32480
     8     ' 2-3.056690E+00 1.6987E+01-2.607930E+00 9.9081E+00',         LN32490
     8     '-2.288910E+00 6.5692E+00-2.044900E+00 4.7254E+00-1'/         LN32500
      DATA CPL269/                                                       LN32510
     9     ' 21  741.722273 1.948E-23 2.484E-02.0892.1173 1337',         LN32520
     9     '.48150.75 0.00000  8  4             Q  2 421    -1',         LN32530
     9     ' 2-1.531530E+01-7.5414E+01-1.215279E+01-4.8229E+01',         LN32540
     9     '-1.018316E+01-3.4242E+01-8.824659E+00-2.5918E+01-1',         LN32550
     *     ' 21  791.452651 7.014E-24 3.306E-03.0892.1173 1287',         LN32560
     *     '.75120.75 0.00000  8  3             Q  2 421    -1',         LN32570
     *     ' 2 5.116410E+00-8.6767E+00 4.055350E+00-5.5549E+00',         LN32580
     *     ' 3.395340E+00-3.9463E+00 2.940600E+00-2.9880E+00-1',         LN32590
     1     ' 21  791.464588 1.228E-23 5.946E-03.0876.1146 1293',         LN32600
     1     '.21790.75 0.00000  8  3             Q  4 421    -1',         LN32610
     1     ' 2 7.640489E-01 3.0051E+00 6.799909E-01 1.7842E+00',         LN32620
     1     ' 6.114030E-01 1.2000E+00 5.543720E-01 8.7278E-01-1',         LN32630
     2     ' 21  791.483374 1.699E-23 8.575E-03.0860.1136 1301',         LN32640
     2     '.80820.75 0.00000  8  3             Q  6 421    -1',         LN32650
     2     ' 2 7.558720E-02 9.1463E-01 1.247753E-01 5.7294E-01',         LN32660
     2     ' 1.415440E-01 3.9916E-01 1.453790E-01 2.9676E-01-1'/         LN32670
      DATA CPL273/                                                       LN32680
     3     ' 21  791.509047 2.095E-23 1.119E-02.0843.1129 1313',         LN32690
     3     '.52220.75 0.00000  8  3             Q  8 421    -1',         LN32700
     3     ' 2-1.342250E-01 3.3225E-01-5.237830E-02 2.2157E-01',         LN32710
     3     '-1.268878E-02 1.6046E-01 8.724430E-03 1.2226E-01-1',         LN32720
     4     ' 21  791.541659 2.401E-23 1.379E-02.0826.1121 1328',         LN32730
     4     '.35940.75 0.00000  8  3             Q 10 421    -1',         LN32740
     4     ' 2-2.155530E-01 1.3105E-01-1.253746E-01 9.5594E-02',         LN32750
     4     '-7.851090E-02 7.2808E-02-5.085989E-02 5.7247E-02-1',         LN32760
     5     ' 21  791.581276 2.611E-23 1.636E-02.0810.1113 1346',         LN32770
     5     '.31970.75 0.00000  8  3             Q 12 421    -1',         LN32780
     5     ' 2-2.453100E-01 4.4574E-02-1.557400E-01 3.8916E-02',         LN32790
     5     '-1.077180E-01 3.2197E-02-7.831070E-02 2.6541E-02-1',         LN32800
     6     ' 21  791.627977 2.724E-23 1.890E-02.0793.1103 1367',         LN32810
     6     '.40260.75 0.00000  8  3             Q 14 421    -1',         LN32820
     6     ' 2-2.538250E-01 1.2476E-02-1.678820E-01 1.6705E-02',         LN32830
     6     '-1.208935E-01 1.5821E-02-9.148619E-02 1.3968E-02-1'/         LN32840
      DATA CPL277/                                                       LN32850
     7     ' 21  791.681854 2.743E-23 2.142E-02.0777.1092 1391',         LN32860
     7     '.60780.75 0.00000  8  3             Q 16 421    -1',         LN32870
     7     ' 2-2.518360E-01-4.4882E-03-1.706770E-01 4.1707E-03',         LN32880
     7     '-1.257048E-01 6.2223E-03-9.713080E-02 6.4126E-03-1',         LN32890
     8     ' 21  791.743015 2.680E-23 2.390E-02.0761.1078 1418',         LN32900
     8     '.93480.75 0.00000  8  3             Q 18 421    -1',         LN32910
     8     ' 2-2.451150E-01-1.0301E-02-1.689610E-01-8.4595E-04',         LN32920
     8     '-1.263184E-01 2.1109E-03-9.891439E-02 3.0579E-03-1',         LN32930
     9     ' 21  791.811579 2.548E-23 2.633E-02.0747.1063 1449',         LN32940
     9     '.38310.75 0.00000  8  3             Q 20 421    -1',         LN32950
     9     ' 2-2.365090E-01-1.4752E-02-1.651520E-01-4.8327E-03',         LN32960
     9     '-1.248611E-01-1.2333E-03-9.872329E-02 2.7820E-04-1',         LN32970
     *     ' 21  791.887680 2.361E-23 2.873E-02.0733.1045 1482',         LN32980
     *     '.95210.75 0.00000  8  3             Q 22 421    -1',         LN32990
     *     ' 2-2.271490E-01-1.4947E-02-1.602640E-01-5.7658E-03',         LN33000
     *     '-1.222247E-01-2.2577E-03-9.735440E-02-6.7427E-04-1'/         LN33010
      DATA CPL281/                                                       LN33020
     1     ' 21  791.971464 2.137E-23 3.107E-02.0721.1026 1519',         LN33030
     1     '.64110.75 0.00000  8  3             Q 24 421    -1',         LN33040
     1     ' 2-2.166710E-01-1.5411E-02-1.541540E-01-6.7707E-03',         LN33050
     1     '-1.183741E-01-3.3024E-03-9.482979E-02-1.6362E-03-1',         LN33060
     2     ' 21  792.063093 1.891E-23 3.336E-02.0710.1005 1559',         LN33070
     2     '.44950.75 0.00000  8  3             Q 26 421    -1',         LN33080
     2     ' 2-2.068820E-01-1.5115E-02-1.482520E-01-7.1338E-03',         LN33090
     2     '-1.145092E-01-3.8225E-03-9.217779E-02-2.1683E-03-1',         LN33100
     3     ' 21  792.162740 1.638E-23 3.560E-02.0699.0983 1602',         LN33110
     3     '.37640.75 0.00000  8  3             Q 28 421    -1',         LN33120
     3     ' 2-1.968070E-01-1.3721E-02-1.419080E-01-6.6511E-03',         LN33130
     3     '-1.101698E-01-3.6722E-03-8.905130E-02-2.1575E-03-1',         LN33140
     4     ' 21  792.270590 1.390E-23 3.777E-02.0690.0960 1648',         LN33150
     4     '.42100.75 0.00000  8  3             Q 30 421    -1',         LN33160
     4     ' 2-1.869140E-01-1.3118E-02-1.354730E-01-6.6366E-03',         LN33170
     4     '-1.056094E-01-3.8434E-03-8.564790E-02-2.3876E-03-1'/         LN33180
      DATA CPL285/                                                       LN33190
     5     ' 21  792.386845 1.156E-23 3.988E-02.0681.0935 1697',         LN33200
     5     '.58260.75 0.00000  8  3             Q 32 421    -1',         LN33210
     5     ' 2-1.777750E-01-1.2004E-02-1.295203E-01-6.2085E-03',         LN33220
     5     '-1.013831E-01-3.6774E-03-8.249800E-02-2.3388E-03-1',         LN33230
     6     ' 21  792.511715 9.423E-24 4.192E-02.0672.0910 1749',         LN33240
     6     '.86000.75 0.00000  8  3             Q 34 421    -1',         LN33250
     6     ' 2-1.693900E-01-1.0817E-02-1.239550E-01-5.6627E-03',         LN33260
     6     '-9.736350E-02-3.3930E-03-7.944690E-02-2.1819E-03-1',         LN33270
     7     ' 21  792.645428 7.539E-24 4.390E-02.0664.0885 1805',         LN33280
     7     '.25240.75 0.00000  8  3             Q 36 421    -1',         LN33290
     7     ' 2-1.608230E-01-9.8674E-03-1.181492E-01-5.2426E-03',         LN33300
     7     '-9.309039E-02-3.1869E-03-7.614879E-02-2.0790E-03-1',         LN33310
     8     ' 21  792.788220 5.919E-24 4.579E-02.0657.0859 1863',         LN33320
     8     '.75860.75 0.00000  8  3             Q 38 421    -1',         LN33330
     8     ' 2-1.528540E-01-9.1400E-03-1.127022E-01-4.9452E-03',         LN33340
     8     '-8.905649E-02-3.0592E-03-7.301710E-02-2.0309E-03-1'/         LN33350
      DATA CPL289/                                                       LN33360
     9     ' 21  792.940342 4.562E-24 4.761E-02.0649.0833 1925',         LN33370
     9     '.37750.75 0.00000  8  3             Q 40 421    -1',         LN33380
     9     ' 2-1.454440E-01-8.0413E-03-1.076049E-01-4.3536E-03',         LN33390
     9     '-8.526310E-02-2.6924E-03-7.006220E-02-1.7846E-03-1',         LN33400
     *     ' 21  793.102057 3.453E-24 4.935E-02.0643.0808 1990',         LN33410
     *     '.10810.75 0.00000  8  3             Q 42 422    -1',         LN33420
     *     ' 2-1.381120E-01-7.4499E-03-1.024881E-01-4.0929E-03',         LN33430
     *     '-8.140209E-02-2.5665E-03-6.702019E-02-1.7244E-03-1',         LN33440
     1     ' 21  793.273640 2.567E-24 5.100E-02.0637.0784 2057',         LN33450
     1     '.94890.75 0.00000  8  3             Q 44 422    -1',         LN33460
     1     ' 2-1.314430E-01-6.7694E-03-9.786400E-02-3.7518E-03',         LN33470
     1     '-7.793630E-02-2.3709E-03-6.430840E-02-1.6045E-03-1',         LN33480
     2     ' 21  793.455378 1.874E-24 5.257E-02.0631.0761 2128',         LN33490
     2     '.89870.75 0.00000  8  3             Q 46 422    -1',         LN33500
     2     ' 2-1.251744E-01-6.1331E-03-9.345569E-02-3.4215E-03',         LN33510
     2     '-7.460050E-02-2.1744E-03-6.168110E-02-1.4788E-03-1'/         LN33520
      DATA CPL293/                                                       LN33530
     3     ' 21  793.647570 1.345E-24 5.404E-02.0625.0739 2202',         LN33540
     3     '.95620.75 0.00000  8  3             Q 48 422    -1',         LN33550
     3     ' 2-1.187953E-01-5.5149E-03-8.897330E-02-3.0939E-03',         LN33560
     3     '-7.121530E-02-1.9756E-03-5.902260E-02-1.3490E-03-1',         LN33570
     4     ' 21  793.850527 9.481E-25 5.542E-02.0619.0718 2280',         LN33580
     4     '.12000.75 0.00000  8  3             Q 50 422    -1',         LN33590
     4     ' 2-1.136564E-01-5.0120E-03-8.542559E-02-2.8270E-03',         LN33600
     4     '-6.859450E-02-1.8134E-03-5.701540E-02-1.2429E-03-1',         LN33610
     5     ' 21  794.064570 6.569E-25 5.671E-02.0613.0699 2360',         LN33620
     5     '.38860.75 0.00000  8  3             Q 52 422    -1',         LN33630
     5     ' 2-1.083940E-01-4.5349E-03-8.181550E-02-2.5709E-03',         LN33640
     5     '-6.595290E-02-1.6562E-03-5.501990E-02-1.1396E-03-1',         LN33650
     6     ' 21  794.290032 4.473E-25 5.790E-02.0607.0682 2443',         LN33660
     6     '.76050.75 0.00000  8  3             Q 54 422    -1',         LN33670
     6     ' 2-1.037816E-01-4.1081E-03-7.880600E-02-2.3418E-03',         LN33680
     6     '-6.388590E-02-1.5162E-03-5.357170E-02-1.0480E-03-1'/         LN33690
      DATA CPL297/                                                       LN33700
     7     ' 21  794.527259 2.994E-25 5.899E-02.0601.0667 2530',         LN33710
     7     '.23400.75 0.00000  8  3             Q 56 422    -1',         LN33720
     7     ' 2-1.003418E-01-3.7461E-03-7.698080E-02-2.1558E-03',         LN33730
     7     '-6.297980E-02-1.4091E-03-5.324540E-02-9.8286E-04-1',         LN33740
     8     ' 21  794.776606 1.970E-25 5.997E-02.0595.0654 2619',         LN33750
     8     '.80760.75 0.00000  8  3             Q 58 422    -1',         LN33760
     8     ' 2-1.003717E-01-3.5602E-03-7.855120E-02-2.1121E-03',         LN33770
     8     '-6.530420E-02-1.4208E-03-5.595850E-02-1.0181E-03-1',         LN33780
     9     ' 21  795.038438 1.275E-25 6.085E-02.0589.0643 2712',         LN33790
     9     '.47950.75 0.00000  8  3             Q 60 422    -1',         LN33800
     9     ' 2-1.210092E-01-5.6705E-03-9.830080E-02-3.7712E-03',         LN33810
     9     '-8.385520E-02-2.7622E-03-7.329530E-02-2.1217E-03-1'/         LN33820
C                                                                        LN33830
      END                                                                LN33840
