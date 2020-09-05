       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EX08.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     18/05/2020.
       DATE-COMPILED.
       SECURITY.  APENAS O AUTOR PODE MODIFICA-LO.    
      *REMARKS.  LE OS REGISTRO DO ARQUIVO DE
      *          ENTRADA CADFUN E GRAVA NO
      *          ARQUIVO DE SAIDA CADSAI
      *          OS REGISTRO COM SALARIO REAJUSTADO.
       ENVIRONMENT             DIVISION.
       CONFIGURATION SECTION.
             SOURCE-COMPUTER.  MARIA-PC.
             OBJECT-COMPUTER.  MARIA-PC.
             SPECIAL-NAMES.         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT   CADFUN ASSIGN    TO    DISK
           ORGANIZATION IS  LINE      SEQUENTIAL.

           SELECT   CADSAI ASSIGN   TO      DISK
           ORGANIZATION IS  LINE      SEQUENTIAL.

       DATA                           DIVISION.

       FILE SECTION.
       FD  CADFUN
           LABEL  RECORD     ARE     STANDARD
           VALUE  OF FILE-ID IS     "CADFUN.DAT".
      
       01  REG-ENT.
             02 CODIGO-ENT              PIC 9(05).
             02 NOME-ENT                PIC X(20).
             02 SALARIO-BRUTO-ENT       PIC 9(5)V99.
       FD  CADSAI
           LABEL  RECORD      ARE   STANDARD
           VALUE  OF FILE-ID  IS    "CADSAI1.DAT".
       
       01   REG-SAI.
            03 CODIGO-SAI                PIC 9(05).
            03 NOME-SAI                  PIC X(20).
            03 SALARIO-REAJUSTADO-SAI    PIC 9(5)V99.

       WORKING-STORAGE SECTION.

       77  FIM-ARQ    PIC X(03)     VALUE   "NAO".
       77  WMULT      PIC 9(5)V99.
       77  WDIV       PIC 9(5)V99.
       77  WRESULTADO PIC 9(5)V99.
       
       PROCEDURE                     DIVISION.
       
       PGM-EX08.

	       PERFORM    INICIO.

	       PERFORM    PRINCIPAL 
                    UNTIL      FIM-ARQ   EQUAL     "SIM".
      	 
	       PERFORM   TERMINO.
	  
           STOP RUN.

       INICIO.

           OPEN INPUT        CADFUN
               OUTPUT        CADSAI.

           PERFORM       LEITURA.      


       LEITURA.

           READ CADFUN
                    AT END 
                    MOVE    "SIM"    TO   FIM-ARQ.

       PRINCIPAL.

	       PERFORM             SELECAO.
           PERFORM             DIVIDIR.
           PERFORM             SOMA.
           PERFORM             GRAVACAO.
	       PERFORM             LEITURA.


       SELECAO.

           IF    SALARIO-BRUTO-ENT >= 1 AND SALARIO-BRUTO-ENT <= 1000
	             MULTIPLY SALARIO-BRUTO-ENT BY 12 GIVING WMULT.

           IF    SALARIO-BRUTO-ENT > 1000 AND SALARIO-BRUTO-ENT <= 2000
                 MULTIPLY SALARIO-BRUTO-ENT BY 11 GIVING WMULT.

            IF   SALARIO-BRUTO-ENT > 2000
                 MULTIPLY SALARIO-BRUTO-ENT BY 10 GIVING WMULT.         

       DIVIDIR.
                 DIVIDE WMULT BY 100 GIVING WDIV.

       SOMA. 
                 ADD  SALARIO-BRUTO-ENT WDIV GIVING WRESULTADO.      


       GRAVACAO.

           MOVE      CODIGO-ENT     TO          CODIGO-SAI.
           MOVE      NOME-ENT       TO          NOME-SAI.
           MOVE      WRESULTADO     TO          SALARIO-REAJUSTADO-SAI.
           WRITE     REG-SAI.


       TERMINO.
           CLOSE       CADFUN
                       CADSAI.
                      

