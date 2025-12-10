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
           select INPUT-FILE assign to 'EMP-DATA.TXT'