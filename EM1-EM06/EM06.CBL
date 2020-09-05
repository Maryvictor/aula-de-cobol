       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EX06.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     18/05/2020.
       DATE-COMPILED.
       SECURITY.  APENAS O AUTOR PODE MODIFICA-LO.    
      *REMARKS.  LE OS REGISTRO DO ARQUIVO DE
      *          ENTRADA CADENT E GRAVA NO
      *          ARQUIVO DE SAIDA CADSAI APENAS
      *          OS REGISTRO COM SALARIO ACIMA DE 3000.
       ENVIRONMENT             DIVISION.
       CONFIGURATION SECTION.
             SOURCE-COMPUTER.  MARIA-PC.
             OBJECT-COMPUTER.  MARIA-PC.
             SPECIAL-NAMES.         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT   CADENT ASSIGN    TO    DISK
           ORGANIZATION IS  LINE      SEQUENTIAL.

           SELECT   CADSAI ASSIGN   TO      DISK
           ORGANIZATION IS  LINE      SEQUENTIAL.

       DATA                           DIVISION.

       FILE SECTION.
       FD  CADENT
           LABEL  RECORD     ARE     STANDARD
           VALUE  OF FILE-ID IS     "CADENT.DAT".
      
       01  REG-ENT.
             02 MATRICULA-ENT       PIC 9(05).
             02 NOME-ENT            PIC X(30).
             02 SALARIO-BRUTO-ENT   PIC 9(5)V99.
       FD  CADSAI
           LABEL  RECORD      ARE   STANDARD
           VALUE  OF FILE-ID  IS    "CADSAI.DAT".
       
       01   REG-SAI.
            03 MATRICULA-SAI        PIC 9(05).
            03 NOME-SAI             PIC X(30).
            03 SALARIO-BRUTO-SAI    PIC 9(5)V99.

       WORKING-STORAGE SECTION.

       77  FIM-ARQ    PIC X(03)     VALUE   "NAO".
       
       PROCEDURE                     DIVISION.
       
       PGM-EX06.

	       PERFORM    INICIO.

	       PERFORM    PRINCIPAL 
                    UNTIL      FIM-ARQ   EQUAL     "SIM".
      	 
	       PERFORM   TERMINO.
	  
           STOP RUN.

       INICIO.

           OPEN INPUT        CADENT
               OUTPUT        CADSAI.

           PERFORM       LEITURA.      


       LEITURA.

           READ CADENT
                    AT END 
                    MOVE    "SIM"    TO   FIM-ARQ.

       PRINCIPAL.

	       PERFORM             SELECAO.
	       PERFORM             LEITURA.


       SELECAO.

           IF    SALARIO-BRUTO-ENT >  3000
	             PERFORM      GRAVACAO.

       GRAVACAO.

           MOVE      MATRICULA-ENT     TO          MATRICULA-SAI.
           MOVE      NOME-ENT          TO          NOME-SAI.
           MOVE      SALARIO-BRUTO-ENT TO          SALARIO-BRUTO-SAI.
           WRITE     REG-SAI.


       TERMINO.
           CLOSE       CADENT
                       CADSAI.
                      

