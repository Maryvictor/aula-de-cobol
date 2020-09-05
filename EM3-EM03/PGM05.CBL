       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PGM05.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     09/06/2020.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.    
      *REMARKS.  LE OS REGISTRO DO ARQUIVO DE
      *          ENTRADA CADENT, E GRAVA OS REGISTROS DE PESSOAS COM 
      *          IDADE MAIOR OU IGUAL QUE 18 ANOS NO CADFESTA.
                
       ENVIRONMENT             DIVISION.
       
       CONFIGURATION SECTION.
             SOURCE-COMPUTER. MARIA-PC.
             OBJECT-COMPUTER. MARIA-PC.
             SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT   CADENT       ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.
           SELECT   TRAB          ASSIGN      TO      DISK.

           SELECT   CADFESTA           ASSIGN      TO      DISK
           ORGANIZATION    IS         LINE          SEQUENTIAL.
       DATA                           DIVISION.

       FILE SECTION.
       FD  CADENT
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CADENT.DAT".
      
       01  REG-ENT.
           02 NOME-ENT      PIC X(30).
           02 IDADE-ENT     PIC 9(02).
           02 RG-ENT        PIC X(10).

       SD  TRAB.
       01  REG-TRA.
           02 NOME-TRAB      PIC X(30).
           02 IDADE-TRAB     PIC 9(02).
           02 RG-TRAB        PIC X(10).
           
                
             
       FD  CADFESTA
           LABEL         RECORD         ARE        STANDARD
           VALUE        OF FILE-ID      IS       "CADFESTA.DAT".
       
       01   REG-SAI.
            03 NOME-SAI      PIC X(30).
            03 IDADE-SAI     PIC 9(02).
            03 RG-SAI        PIC X(10).
            
            

       WORKING-STORAGE SECTION. 

       77    FIM-ARQ          PIC X(03) VALUE "NAO".    

       
       PROCEDURE                                   DIVISION.
       
       PGM-05.

	       SORT    TRAB
                   ASCENDING KEY NOME-TRAB
                   INPUT PROCEDURE ROT-ENTRADA
                   GIVING CADFESTA.
            STOP RUN. 

       ROT-ENTRADA      SECTION.
           PERFORM INICIO-ENTRADA.
           PERFORM PRINCIPAL-ENTRADA
                UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM FIM-ENTRADA.

       INICIO-ENTRADA SECTION.
           OPEN INPUT CADENT.
           PERFORM LEITURA-ENTRADA.



       LEITURA-ENTRADA   SECTION.
           READ CADENT
                AT END
                MOVE "SIM"  TO FIM-ARQ.

       PRINCIPAL-ENTRADA    SECTION.
           PERFORM SELECAO-ENTRADA.
           PERFORM LEITURA-ENTRADA.

       SELECAO-ENTRADA    SECTION.
           IF IDADE-ENT >= 18
               PERFORM GRAVACAO-ENTRADA.

       GRAVACAO-ENTRADA SECTION.
           MOVE NOME-ENT  TO NOME-TRAB.
           MOVE IDADE-ENT TO IDADE-TRAB.
           MOVE RG-ENT    TO RG-TRAB.
           RELEASE REG-TRA.

       FIM-ENTRADA  SECTION.
           CLOSE   CADENT.



                      

