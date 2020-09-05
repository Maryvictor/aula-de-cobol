       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX05.
       AUTHOR. MARIA VICTOR.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 18/05/2020.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO. 
      *REMARKS. LE OS REGISTRO DO ARQUIVO DE
      *         ENTRADA CADALU, CALCULA A MEDIA
      *         E GRAVA O ARQUIVO DE SAIDA CADAPR SOMENTE OS ARQUIVOS 
      *         COM MEDIA >= 7 E FALTAS <= 18.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MARIA-PC.
       OBJECT-COMPUTER. MARIA-PC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADAPR ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION. 
       FILE SECTION.
       FD CADALU 
          LABEL    RECORD        ARE        STANDARD
          VALUE   OF FILE-ID     IS        "CADALU2.DAT".
       01 REG-ENT. 
          03 NUMERO-ENT   PIC 9(05).
          03 NOME-ENT     PIC X(20).
          03 NOTA1-ENT    PIC 9(02)V9(02).
          03 NOTA2-ENT    PIC 9(02)V99.
          03 FALTAS-ENT   PIC X(02).

       FD CADAPR
          LABEL     RECORD       ARE         STANDARD
          VALUE     OF FILE-ID   IS         "CADAPR.DAT".
       01 REG-SAI.
          03 NUMERO-SAI   PIC 9(05).
          03 NOME-SAI     PIC X(20).
          03 MEDIA-SAI    PIC 9(02)V99.
      
       WORKING-STORAGE SECTION.
       77 FIM-ARQ    PIC X(03)       VALUE "NAO".
       77 SOMA-NOTA  PIC 9(02)V9(02) VALUE ZEROS.
       77 WMEDIA     PIC 99V99       VALUE ZEROS.

       PROCEDURE DIVISION.
       PGM-EX05. 
           PERFORM INICIO.
           PERFORM PRINCIPAL 
                   UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.
       
       INICIO. 
           OPEN INPUT  CADALU 
                OUTPUT CADAPR.
           PERFORM LEITURA.
       
       LEITURA. 
           READ CADALU 
                AT END 
                MOVE "SIM" TO FIM-ARQ.
    
       PRINCIPAL.
           PERFORM CALCMEDIA.
           PERFORM SELECAO.
           PERFORM LEITURA.
      
       CALCMEDIA. 
           ADD NOTA1-ENT NOTA2-ENT GIVING SOMA-NOTA. 
           DIVIDE SOMA-NOTA BY 2 GIVING WMEDIA.

       SELECAO.
           IF       WMEDIA       >= 7 AND  FALTAS-ENT <= 18
                    PERFORM        GRAVACAO.

       GRAVACAO.   
           MOVE  NUMERO-ENT TO NUMERO-SAI.
           MOVE  NOME-ENT   TO NOME-SAI.
           MOVE  WMEDIA     TO MEDIA-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADALU
                 CADAPR.
