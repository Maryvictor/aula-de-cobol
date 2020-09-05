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
           VALUE        OF FILE-ID       IS      "ENT.DAT".
      
       01  REG-ENT.
           02 COD-ENT       PIC 9(04).
           02 NOME-ENT      PIC X(30).

       SD  TRAB.
       01  REG-TRA.
           02 COD-TRAB        PIC X(04).
           02 NOME-TRAB       PIC X(30).
                
             
       FD  SAIDA
           LABEL         RECORD         ARE        STANDARD
           VALUE        OF FILE-ID      IS       "SAI.DAT".
       
       01   REG-SAI.
            03 COD-SAI       PIC 9(04).
            03 NOME-SAI      PIC X(30).
            

       WORKING-STORAGE SECTION.

       
       PROCEDURE                                   DIVISION.
       
       PGM-EXE01.

	       SORT    TRAB
                   ASCENDING KEY COD-TRAB
                   USING ENTRADA
                   GIVING SAIDA.
            STOP RUN.       

                      

