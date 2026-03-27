       identification division.
         program-id. emp-report.
         author. Simon Vargas.
         date-written. June 10, 2024.
       
      * 
      *--------------------------------------------------------*
      * proj: section 15.4 - Employee Report Program
      * desc: This program demonstates the following concepts: 
      *       - COBOL STRING statement
      *       - COBOL UNSTRING statement
      *       - COBOL INSPECT statement
      *       - COBOL REFERENCE modification
      *    Files:
      *          *INPUT-FILE - Employee data input file(Sequential)
      *          *OUTPUT-FILE - Monthly employee report output file)       
      * Note: This program does not have any exception handling for
      *       simplicity purposes.
      *--------------------------------------------------------*

       environment division.
       input-output section.
       file-control.
           select INPUT-FILE assign to '/Users/simonvargas/emp.dat'
               organization is line sequential 
               file status is WS-FILE-STATUS.

           select OUTPUT-FILE assign to '/Users/simonvargas/rep.dat'
               organization is line sequential 
               file status is WS-FILE-STATUS.

       data division.
       file section.
       fd  INPUT-FILE.
       01  in-emprec.
              05  in-empno           pic 9(6).
              05  in-firstname       pic x(12).
              05  in-midinit         pic x(01).
              05  in-lastname        pic x(17).
              05  in-workdept        pic x(03).
              05  in-phoneno         pic 9(03).
              05  in-hiredate        pic 9(08). 
              05  in-jobty           pic x(08).
              05  in-edlevel         pic 9(02).
              05  in-sex             pic(01).
              05  in-birthdate       pic 9(08).
              05  in-salary          pic 9(07)v99.
              05  in-bonus           pic 9(07)v99.
              05  in-comm            pic 9(07)v99.
              05  filler             pic x(01).
              05  in-add             pic x(48).
              05. filler             pic x(04).

       fd  OUTPUT-FILE.
       01  rep-file-rec.             pic x(150).

       working-storage section.

      * cobol level 88 condition clause.
       01  ws-switch.
           05 end-of-file-switch     pic x(01) value 'n'.
              88 end-of-file                   value 'y'.
              88 not-end-of-file               value 'n'.

      * Temporary variables.
       01  ws-temp-pgm-vals.
           05 ws-emply-name          pic x(30) value spaces.
           05 ws-pointer-fld-1       pic s9(03) value zeros.
           05 ws-emply-add.
              10 ws-add-ln1          pic x(20) value spaces.
              10 ws-add-eircde       pic x(08) value spaces.
              10 WS-ADD-CONTY         PIC X(10)       VALUE SPACES.
              10 WS-ADD-CONTRY-CDE    PIC X(03)       VALUE SPACES.
           05 WS-POINTER-FLD-2       PIC S9(03)      VALUE ZEROES.
           05 WS-EFILE-ST            PIC 9(02)       VALUE ZEROES. 
           05 WS-RFILE-ST            PIC 9(02)       VALUE ZEROES.

       01 WS-COUNTERS.
           05 WS-INP-REC             PIC 9(05).
           05 WS-OUT-REC             PIC 9(05).
           05 WS-SKIP-REC            PIC 9(05).

       01 WS-TEMP-DATE.
           05 WS-HDTE                 PIC 9(08).
           05 THIS REDEFINES WS-HDTE.
              10 WS-HDTE-DD           PIC X(02).
              10 WS-HDTE-MM           PIC X(02).
              10 WS-HDTE-YYYY         PIC X(04).
           05 WS-TDY-DTE PIC 9(08).
           05 THIS REDEFINES WS-TDY-DTE.
              10 WS-TDYDTE-YYYY       PIC 9(04).
              10 WS-TDYDTE-MM         PIC 9(02).
              10 WS-TDYDTE-DD         PIC 9(02).
           66 WS-TODAYS-DTE RENAMES WS-TDYDTE-YYYY THRU WS-TDYDTE-DD.

       01 HEAD1.
           05 FILLER                 PIC X(60)       VALUE SPACES.
           05 FILLER                 PIC X(10) VALUE ' EMPLOYEE '.
           05 FILLER                 PIC X(25) 
                                             VALUE 'MANAGEMENT SYSTEM.'.
           05 FILLER                 PIC X(41) VALUE SPACES.
           05 HD-DTE.
              10 HD-DTE-DD            PIC X(02) VALUE SPACES.
              10 FILLER               PIC X(01) VALUE '/'.
              10 HD-DTE-MM            PIC X(02) VALUE SPACES.
              10 FILLER               PIC X(01) VALUE '/'.
              10 HD-DTE-YYYY          PIC X(04) VALUE SPACES.
              10 FILLER               PIC X(01) VALUE '.'.
              10 FILLER               PIC X(03) VALUE SPACES.    

       01 HEAD2.
           05 FILLER                 PIC X(70) VALUE SPACES.
           05 FILLER                 PIC X(11) VALUE 'DUBLIN, IRL'.
           05 FILLER                 PIC X(69) VALUE SPACES.

       01 COLHEAD3.
           05 FILLER                 PIC X(04) VALUE SPACES.
           05 FILLER                 PIC X(05) VALUE 'EMPNO'.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 FILLER                 PIC X(06) VALUE '  EMP '.
           05 FILLER                 PIC X(15) VALUE 'FULL NAME'.
           05 FILLER                 PIC X(18) VALUE SPACES.
           05 FILLER                 PIC X(06) VALUE 'ADD LN'.
           05 FILLER                 PIC X(09) VALUE SPACES.
           05 FILLER                 PIC X(06) VALUE 'COUNTY'.
           05 FILLER                 PIC X(04) VALUE SPACES.
           05 FILLER                 PIC X(08) VALUE 'EIR CODE'.
           05 FILLER                 PIC X(03) VALUE SPACES.
           05 FILLER                 PIC X(12) VALUE 'COUNTRY CODE'.
           05 FILLER                 PIC X(04) VALUE SPACES.
           05 FILLER                 PIC X(08) VALUE 'ED LEVEL'.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 FILLER                 PIC X(03) VALUE 'SEX'.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 FILLER                 PIC X(10) VALUE 'BIRTH DATE'.
           05 FILLER                 PIC X(06) VALUE SPACES.
           05 FILLER                 PIC X(06) VALUE 'SALARY'.
           05 FILLER                 PIC X(11) VALUE SPACES.

       01 DTL-LINE.
           05 FILLER                 PIC X(03) VALUE SPACES.
           05 DTL-EMPNO              PIC Z(06).
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 DTL-FULL-NME           PIC X(30).
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 DTL-ADD-LN             PIC X(20).
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 DTL-COUNTY             PIC X(08).
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 DTL-EIR-CDE            PIC X(08).
           05 FILLER                 PIC X(06) VALUE SPACES.
           05 DTL-COUNTRY-CDE        PIC X(03).
           05 FILLER                 PIC X(13) VALUE SPACES.
           05 DTL-EDLEVEL            PIC Z(02).
           05 FILLER                 PIC X(06) VALUE SPACES.
           05 DTL-SEX                PIC X(01).
           05 FILLER                 PIC X(03) VALUE SPACES.
           05 DTL-BIRTHDATE.
             10 DTL-BRTHDATE-DD      PIC 9(02).
             10 FILLER               PIC X(01) VALUE '/'.
             10 DTL-BRTHDATE-MM      PIC 9(02).
             10 FILLER               PIC X(01) VALUE '/'.    
             10 DTL-BRTHDATE-YYYY    PIC 9(04).
           05 FILLER                 PIC X(03) VALUE SPACES.
           05 DTL-SALARY             PIC ZZZZZZ9.99.
           05 FILLER                 PIC X(10) VALUE SPACES.

       01 TRL-LINE.
            05 FILLER                 PIC X(04) VALUE SPACES.
           05 TRL-LINE-MSG           PIC X(27) VALUE SPACES.
           05 TRL-COUNT              PIC 9(05) VALUE ZEROES.
           05 FILLER                 PIC X(114) VALUE SPACES.

       01 RPT-BLK-LNE.
           05 RPT-BLK-AST            PIC X(01) VALUE '*'.
           05 RPT-BLK-SPC            PIC X(149) VALUE SPACES.
       
       procedure division.
       declaratives
       decl-empfile section.
           use after error procedure on emp-file.
       empfile-error.
           display 'EMPLoyee file error - ', WS-EFILE-ST.

       decl-repfile section.
           use after error procedure on rep-file.
       repfile-error.
           display 'Report file error - ', WS-RFILE-ST.
       end-declaratives.