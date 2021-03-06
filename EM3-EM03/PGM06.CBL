       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EXE.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     09/06/2020.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.    
      *REMARKS.  LE OS REGISTRO DO ARQUIVO DE
      *          ENTRADA CADENT, ORDENA POR PELA PONTUACAO MAIOR 
      *          (USANDO SORTE) E GRAVA NO ARQUIVO DE SAIDA CADVEST 
      *          PONTUACAO MAIOR OU IGUAL A 70 APENAS.
                
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

           SELECT   CADVEST           ASSIGN      TO      DISK
           ORGANIZATION    IS         LINE          SEQUENTIAL.
       DATA                           DIVISION.

       FILE SECTION.
       FD  CADENT
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CADENT1.DAT".
      
       01  REG-ENT.
           02 CPF-ENT       PIC 9(11).
           02 NOME-ENT      PIC X(30).
           02 PONT-ENT      PIC 9(03).

       SD  TRAB.
       01  REG-TRA.
           02 CPF-TRAB       PIC 9(11).
           02 NOME-TRAB      PIC X(30).
           02 PONT-TRAB      PIC 9(03).
           
                
             
       FD  CADVEST
           LABEL         RECORD         ARE        STANDARD
           VALUE        OF FILE-ID      IS       "CADVEST.DAT".
       
       01   REG-SAI.
            03 CPF-SAI       PIC 9(11).
            03 NOME-SAI      PIC X(30).
            03 PONT-SAI      PIC 9(03).
            
            

       WORKING-STORAGE SECTION. 

       77    FIM-ARQ          PIC X(03) VALUE "NAO".    

       
       PROCEDURE                                   DIVISION.
       
       PGM-06.

	       SORT    TRAB
                   DESCENDING KEY PONT-TRAB
                   INPUT PROCEDURE ROT-ENTRADA
                   GIVING CADVEST.
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

       SELECAO-ENTRADA      SECTION.
           IF  PONT-ENT    >=    70
               PERFORM GRAVACAO-ENTRADA.

       GRAVACAO-ENTRADA     SECTION.
           MOVE CPF-ENT  TO CPF-TRAB.
           MOVE NOME-ENT TO NOME-TRAB.
           MOVE PONT-ENT TO PONT-TRAB.
           RELEASE REG-TRA.

       FIM-ENTRADA  SECTION.
           CLOSE   CADENT.



                      

