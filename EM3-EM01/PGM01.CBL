       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PGM01.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     02/06/2020.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.    
      *REMARKS.  LE OS REGISTRO DO ARQUIVO DE
      *          ENTRADA CADENT, ORDENA POR PELO CODIGO (USANDO SORTE) 
      *          E GRAVA NO ARQUIVO DE SAIDA CADFILME.
                
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

           SELECT   CADFILME           ASSIGN      TO      DISK
           ORGANIZATION    IS         LINE          SEQUENTIAL.
       DATA                           DIVISION.

       FILE SECTION.
       FD  CADENT
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CADENT.DAT".
      
       01  REG-ENT.
             02 COD-ENT       PIC 9(04).
             02 NOME-ENT      PIC X(30).
             02 PRECO-ENT     PIC 9(2)V99.

       SD  TRAB.
       01  REG-TRA.
           02 COD-TRAB        PIC X(04).
           02 NOME-TRAB       PIC X(30).
           02 PRECO-TRAB      PIC 9(2)V99.
                
             
       FD  CADFILME
           LABEL         RECORD         ARE        STANDARD
           VALUE        OF FILE-ID      IS       "CADFILME.DAT".
       
       01   REG-SAI.
            03 COD-SAI       PIC 9(04).
            03 NOME-SAI      PIC X(30).
            03 PRECO-SAI     PIC 9(02)V99.
            

       WORKING-STORAGE SECTION.

       
       PROCEDURE                                   DIVISION.
       
       PGM-EXE01.

	       SORT    TRAB
                   ASCENDING KEY COD-TRAB
                   USING CADENT
                   GIVING CADFILME.
            STOP RUN.       

                      

