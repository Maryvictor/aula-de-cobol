       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PGM02.
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

           SELECT   CADENT       ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.
           SELECT   TRAB          ASSIGN      TO      DISK.

           SELECT   CADLIVRO           ASSIGN      TO      DISK
           ORGANIZATION    IS         LINE          SEQUENTIAL.
       DATA                           DIVISION.

       FILE SECTION.
       FD  CADENT
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CADENT1.DAT".
      
       01  REG-ENT.
           02 COD-ENT       PIC 9(05).
           02 NOME-ENT      PIC X(30).
           02 PRECO-ENT     PIC 9(2)V99.

       SD  TRAB.
       01  REG-TRA.
           02 COD-TRAB       PIC 9(05).
           02 NOME-TRAB      PIC X(30).
           02 PRECO-TRAB     PIC 9(2)V99.
                
             
       FD  CADLIVRO
           LABEL         RECORD         ARE        STANDARD
           VALUE        OF FILE-ID      IS       "CADLIVRO.DAT".
       
       01   REG-SAI.
            03 COD-SAI       PIC 9(05).
            03 NOME-SAI      PIC X(30).
            03 PRECO-SAI     PIC 9(02)V99.
            

       WORKING-STORAGE SECTION.

       
       PROCEDURE                                   DIVISION.
       
       PGM-EXE02.

	       SORT    TRAB
                   DESCENDING KEY NOME-TRAB
                   USING CADENT
                   GIVING CADLIVRO.
            STOP RUN.       

                      

