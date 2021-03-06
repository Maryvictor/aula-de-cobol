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

           SELECT   CADCLI1           ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.

           SELECT   CADCLI2           ASSIGN      TO      DISK
           ORGANIZATION    IS         LINE          SEQUENTIAL.
       DATA                           DIVISION.

       FILE SECTION.
       FD  CADCLI1
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CADCLI1.DAT".
      
       01  REG-ENT.
             02 COD-ENT       PIC 9(05).
             02 NOME-ENT      PIC X(20).
             
       FD  CADCLI2
           LABEL         RECORD         ARE        STANDARD
           VALUE        OF FILE-ID      IS       "CADCLI2.DAT".
       
       01   REG-SAI.
            03 NOME-SAI       PIC X(20).
            03 COD-SAI        PIC 9(05).
            

       WORKING-STORAGE SECTION.

       77  FIM-ARQ          PIC X(03)             VALUE   "NAO".
       PROCEDURE                                   DIVISION.
       
       PGM-EXE01.

	       PERFORM    INICIO.

	       PERFORM    PRINCIPAL 
                    UNTIL   FIM-ARQ      EQUAL     "SIM".
      	 
	       PERFORM   TERMINO.
	  
           STOP RUN.
       INICIO.

           OPEN INPUT       CADCLI1
               OUTPUT       CADCLI2.

           PERFORM       LEITURA.      


       LEITURA.

           READ CADCLI1
                    AT END 
                    MOVE  "SIM"        TO        FIM-ARQ.
       PRINCIPAL.

	       PERFORM             GRAVACAO.
	       PERFORM             LEITURA.


       GRAVACAO.

           MOVE      COD-ENT      TO          COD-SAI.
           MOVE      NOME-ENT     TO          NOME-SAI.
           WRITE     REG-SAI.


       TERMINO.
           CLOSE       CADCLI1
                       CADCLI2.
                      

