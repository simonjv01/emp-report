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
              05  in-emp-no          pic 9(6).
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