       IDENTIFICATION DIVISION.
       PROGRAM-ID.       BALANCE-LINE.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     02/06/2020.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.    
      *REMARKS.  LE OS REGISTRO DO ARQUIVO DE
      *          ENTRADA CADENT, ORDENA POR PELO NOME DO LIVRO 
      *          (USANDO SORTE) E GRAVA NO ARQUIVO DE SAIDA CADLIVRO.
                
       ENVIRONMENT             DIVISION.
       
       CONFIGURATION SECTION.
             SOURCE-COMPUTER. MARIA-PC.
             OBJECT-COMPUTER. MARIA-PC.
             SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT   ARQA      ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.
           SELECT   ARQB          ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.
           SELECT   ARQC           ASSIGN      TO      DISK
           ORGANIZATION    IS         LINE          SEQUENTIAL.

       DATA                           DIVISION.

       FILE SECTION.
       FD  ARQA
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "ARQA.DAT".
      
       01  REG-ARQA.
           02 COD-ARQA       PIC 9(04).
           02 NOME-ARQA      PIC X(30).
           02 TURMA-ARQA     PIC X(20).

       FD  ARQB
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "ARQB.DAT".
      

       01  REG-ARQB.
           03 COD-ARQB       PIC 9(04).
           03 NOME-ARQB      PIC X(30).
           03 TURMA-ARQB     PIC X(20).
           
                
             
       FD  ARQC 
           LABEL         RECORD         ARE        STANDARD
           VALUE        OF FILE-ID      IS       "ARQC.DAT".
       
       01   REG-ARQC.
            03 COD-ARQC       PIC 9(04).
            03 NOME-ARQC      PIC X(30).
            03 TURMA-ARQC     PIC X(20).
            
            

       WORKING-STORAGE SECTION. 

       77    FIM-ARQ          PIC X(03) VALUE "NAO". 
       77    CH-ARQA          PIC X(04) VALUE SPACES.   
       77    CH-ARQB           PIC X(04) VALUE SPACES.   

       
       PROCEDURE                                   DIVISION.
       
       PGM-EXE.

	       PERFORM INICIO.
           PERFORM PRINCIPAL
                   UNTIL
                   CH-ARQA EQUAL CH-ARQB AND
                   CH-ARQB EQUAL HIGH-VALUES.
           PERFORM FIM.
           STOP    RUN.  


       INICIO.
           OPEN INPUT ARQA
                      ARQB 
               OUTPUT ARQC.
           PERFORM LER-ARQA.
           PERFORM LER-ARQB.

       LER-ARQA.
           READ ARQA
                AT END 
                MOVE HIGH-VALUES TO CH-ARQA.

            IF CH-ARQA EQUAL HIGH-VALUES
                NEXT SENTENCE
            ELSE 
                MOVE COD-ARQA    TO  CH-ARQA.

       LER-ARQB.
           READ ARQB 
                AT END 
                MOVE HIGH-VALUES TO CH-ARQB.

            IF CH-ARQB EQUAL HIGH-VALUES
                NEXT SENTENCE
            ELSE 
                MOVE COD-ARQB    TO  CH-ARQB.
           


       PRINCIPAL.
            IF CH-ARQA EQUAL CH-ARQB 
                PERFORM IGUAL
                PERFORM LER-ARQA
                PERFORM LER-ARQB
            ELSE  
            IF  CH-ARQA LESS CH-ARQB 
                PERFORM ARQA-MENOR
                PERFORM LER-ARQA
             ELSE 
                PERFORM ARQB-MENOR
                PERFORM LER-ARQB.

       IGUAL.
           PERFORM GRAVA-ARQA.
           PERFORM GRAVA-ARQB.

       ARQA-MENOR.
           PERFORM GRAVA-ARQA.
     
       ARQB-MENOR.
           PERFORM GRAVA-ARQB.


       GRAVA-ARQB.
           MOVE    COD-ARQB    TO   COD-ARQC.
           MOVE    NOME-ARQB   TO   NOME-ARQC.
           MOVE    TURMA-ARQB  TO   TURMA-ARQC.
           WRITE   REG-ARQC.

       GRAVA-ARQA.
           MOVE    COD-ARQA    TO   COD-ARQC.
           MOVE    NOME-ARQA   TO   NOME-ARQC.
           MOVE    TURMA-ARQA  TO   TURMA-ARQC.
           WRITE   REG-ARQC.

       FIM.
           CLOSE    ARQA
                    ARQB
                    ARQC.        
    



                      

