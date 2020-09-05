       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EX01.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     18/05/2020.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.    
      *REMARKS.  LE OS REGISTRO DO ARQUIVO DE
      *          ENTRADA CADCLI1 E GRAVA NO
      *          ARQUIVO DE SAIDA CADCLI2 
      *          OS CAMPOS: CODIGO E NOME.
       ENVIRONMENT             DIVISION.
       
       CONFIGURATION SECTION.
             SOURCE-COMPUTER. MARIA-PC.
             OBJECT-COMPUTER. MARIA-PC.
             SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT   ENTRADA       ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.
           SELECT   TRAB          ASSIGN      TO      DISK.

           SELECT   SAIDA           ASSIGN      TO      DISK
           ORGANIZATION    IS         LINE          SEQUENTIAL.
       DATA                           DIVISION.

       FILE SECTION.
       FD  ENTRADA
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "ENT1.DAT".
      
       01  REG-ENT.
           02 COD-ENT       PIC 9(04).
           02 NOME-ENT      PIC X(30).
           02 SEXO-ENT      PIC X(01).

       SD  TRAB.
       01  REG-TRA.
           02 COD-TRAB        PIC 9(04).
           02 NOME-TRAB       PIC X(30).
           02 SEXO-TRAB       PIC X(01).
                
             
       FD  SAIDA
           LABEL         RECORD         ARE        STANDARD
           VALUE        OF FILE-ID      IS       "SAI.DAT".
       
       01   REG-SAI.
            03 COD-SAI       PIC 9(04).
            03 NOME-SAI      PIC X(30).
            03 SEX-SAI       PIC X(01).
            

       WORKING-STORAGE SECTION.

       77     FIM-ARQ         PIC X(03) VALUE  "NAO".

       PROCEDURE                                   DIVISION.
       
       PGM-EXE01.

	       SORT    TRAB
                   ASCENDING KEY COD-TRAB
                   USING ENTRADA
                   OUTPUT PROCEDURE ROT-SAIDA
            STOP RUN.   

       ROT-SAIDA              SECTION.
           PERFORM   INICIO.
           PERFORM   PRINCIPAL
                    UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM FIM.


       INICIO                 SECTION.
           OPEN    OUTPUT SAIDA.
           PERFORM LE-SORT.


       LE-SORT                SECTION.
           RETURN  TRAB
                   AT END
                   MOVE "SIM"  TO   FIM-ARQ.


       PRINCIPAL              SECTION.
            PERFORM  SELECAO.
            PERFORM  LE-SORT.


       SELECAO                 SECTION.
           IF SEXO-TRAB EQUAL "F"
               PERFORM GRAVACAO.

       GRAVACAO                SECTION.
           MOVE COD-TRAB  TO COD-SAI.
           MOVE SEXO-TRAB TO SEX-SAI.
           MOVE NOME-TRAB TO NOME-SAI.
           WRITE REG-SAI.


       FIM                      SECTION.
           CLOSE  SAIDA.                                

