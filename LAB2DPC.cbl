       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT.
       AUTHOR.     JENSEN.
      **********************************************************
      *  This program reads data from an external data file
      *  and adds the number of boxes sold of samoas and mints
      *  for each girl scout
      *
      *  INPUT:  name, number of samoas and mints cookies
      *
      *  OUTPUT: name and original amount read in and total sold
      *
      *  CALCULATIONS: TOTAL = MINTS + SAMOA.
      *
      *LAB INSTRUCTIONS:
      *    YOU ARE TO FIND ANY ERRORS WHICH I MAY HAVE PUT IN THE
      *    PROGRAM.  YOU ARE TO ALSO LOOK FOR ANY COMMENTS I
      *    HAVE INCLUDED WHICH ARE INSTRUCTIONS.  THESE INSTRUCTIONS
      *    WILL TELL YOU WHAT YOU NEED TO INCLUDE IN YOUR PROGRAM
      *    TO MAKE IT WORK.
      **********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
	  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * ASSIGN A FILE NAME FOR THE DISK FILE
      * USE SCOUT.DAT AS THE DISK NAME

           SELECT GRADE-FILE 
				ASSIGN TO 'GRADES.DAT'
                ORGANIZATION IS LINE SEQUENTIAL.

      * USE REPORT.DAT FOR THE PRINTER FILE

           SELECT REPORT-FILE 
				ASSIGN TO 'PRGRADES'.

      *
       DATA DIVISION.
       FILE SECTION.


      * SAME NAME AS THE SELECT STATEMENT

       FD    GRADE-FILE
			 RECORD CONTAINS 80 CHARACTERS.
			 
			 
       01    SCOUT-REC.
			05  GR-NAME			PIC X(10).
			05  GR-FIRST		PIC s999.
			05  GR-SEC			PIC s999.
			05  FILLER			PIC X(48).

      *MUST IDENTIFY AND ACCOUNT FOR ALL FIELDS OF THE INCOMING RECORD.

       FD    REPORT-FILE.
			 RECORD CONTAINS 80 CHARACTERS.
	   
       01    REPORT-REC.
             05                                PIC X(80).

       WORKING-STORAGE SECTION.
       01    WS-WORK-AREAS.
             05    ARE-THERE-MORE-RECORDS    PIC X(3) VALUE 'YES'.


      *DECLARE ANY VARIABLES IN THIS AREA.


       01 DETAIL-LINE.
      *CREATE A PLACE FOR OUTPUT USE 05 LEVELS INCLUDE NAME,
      * NUMBER OF MINTS COMING IN, NUMBER OF SAMOA COMING IN AND
      * TOTAL SOLD FOR EACH GIRL.


       PROCEDURE DIVISION.

       100-MAIN-MODULE.

           PERFORM 125-HOUSKEING
           PERFORM 150-READ-RECORDS.
           PERFORM 250-END-ROUTINE

           .



       125-HOUSEKEEPING.

      *SAME FILE NAME AS SELECT STATEMENT
           OPEN    INPUT
                   OUTPUT    REPORT-FILE
           .


       150-READ-RECORDS.

             PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO'
                  READ SCOUT-RECORD
                      AT END
                          MOVE 'NO' TO ARE-THERE-MORE-RECORDS
                      NOT AT END
                          PERFORM 200-PROCESS
                  END-READ
              END-PERFORM
           .



       200-PROCESS-RTN.

      *MATCH YOUR DATANAMES TO THESE DATANAMES

              MOVE SCOUT-NAME-IN TO SCOUT-NAME-OUT
              MOVE MINT-BOXES-IN TO MINT-BOXES-OUT
              MOVE SAMOA-BOXES-IN TO SAMOA-BOXES-OUT
              ADD MINT-BOXES-IN SAMOA-BOXES-IN GIVING SCOUT-TOTAL-OUT

			  MOVE DETAIL-LINE TO REPORT-REC
			  
              WRITE REPORT-REC
           .

       250-CLOSE-ROUTINE.

      *SAME FILE NAME AS SELECT STATEMENT
              CLOSE
                       REPORT-FILE
              STOP RUN
           .

