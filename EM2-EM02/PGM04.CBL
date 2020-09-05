       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PGM04.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     26/05/2020.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.    
      *REMARKS.  LE UM REGISTRO COM DEZ PARCELAS E SOMA O TOTAL.
                 
              
       ENVIRONMENT             DIVISION.
       
       CONFIGURATION SECTION.
             SOURCE-COMPUTER. MARIA-PC.
             OBJECT-COMPUTER. MARIA-PC.
             SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT   CADPAG           ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.
           SELECT   CADREC           ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.

       DATA      DIVISION.

       FILE SECTION.
       FD  CADPAG
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CADPAG.DAT".
      
       01   REG-ENT.
             02 CODIGO-ENT      PIC 9(05).
             02 NOME-ENT        PIC X(30).
             02 PARCELAS        PIC 9(05)V9(02)
                                 OCCURS 10 TIMES.
       FD  CADREC
           LABEL RECORD ARE  STANDARD.

       01   REG-SAI.
             02 CODIGO-SAI         PIC 9(05).
             02 NOME-SAI           PIC X(30).
             02 TOTALPG-SAI        PIC 9(05)V9(02).
                
            
       WORKING-STORAGE SECTION.

       77  FIM-ARQ          PIC X(03)            VALUE "NAO".
       77  CTR              PIC 9(02)            VALUE ZEROS.
       77  TOTAL            PIC 9(05)V9(02)      VALUE ZEROES.

       PROCEDURE                                   DIVISION.
       
       EM02-PGM04.

	       PERFORM    INICIO.

	       PERFORM    PRINCIPAL 
                    UNTIL   FIM-ARQ      EQUAL     "SIM".
           PERFORM    FIM.         
	  
           STOP RUN.

       INICIO.

           OPEN INPUT       CADPAG
                OUTPUT      CADREC.
           PERFORM   LEITURA.     

            

       LEITURA.
           READ CADPAG
                AT END
                MOVE "SIM" TO FIM-ARQ.
           
       


       PRINCIPAL.

           PERFORM ACUMULA-PG.
           PERFORM GRAVA-SAI.
           PERFORM LEITURA.

       ACUMULA-PG.
           MOVE    ZEROS         TO TOTAL.
           MOVE    1             TO CTR.
           PERFORM SOMA-TAB      10 TIMES.
        


       SOMA-TAB.
           ADD PARCELAS (CTR)    TO TOTAL.
           ADD 1                 TO CTR.

       GRAVA-SAI.
           MOVE CODIGO-ENT       TO CODIGO-SAI.
           MOVE NOME-ENT         TO NOME-SAI.
           MOVE TOTAL            TO TOTALPG-SAI.
           WRITE REG-SAI.      


       FIM.

           CLOSE CADPAG
                 CADREC.            

