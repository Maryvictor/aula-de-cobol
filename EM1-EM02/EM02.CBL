       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EX02.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     18/05/2020.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.    
      *REMARKS.  LE OS REGISTRO DO ARQUIVO DE
      *          ENTRADA CADALU E GRAVA NO
      *          ARQUIVO DE SAIDA CADATU 
      *          OS CAMPOS: NUMERO,NOME E DATA DE NASCIMENTO.
       ENVIRONMENT             DIVISION.
       
       CONFIGURATION SECTION.
             SOURCE-COMPUTER. MARIA-PC.
             OBJECT-COMPUTER. MARIA-PC.
             SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT   CADALU           ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.

           SELECT   CADATU           ASSIGN      TO      DISK
           ORGANIZATION    IS         LINE          SEQUENTIAL.
       DATA                           DIVISION.

       FILE SECTION.
       FD  CADALU
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CADALU.DAT".
      
       01  REG-ENT.
             03 NUMERO-ENT       PIC 9(05).
             03 NOME-ENT         PIC X(20).
             03 SEXO-ENT         PIC X(01).
             03 DATA-NASC-ENT.
                04 DD-ENT        PIC 9(02).
                04 MM-ENT        PIC 9(02).
                04 AAAA-ENT      PIC 9(04).
             
       FD  CADATU
           LABEL         RECORD         ARE        STANDARD
           VALUE        OF FILE-ID      IS       "CADATU.DAT".
       
       01   REG-SAI.
            05 NUMERO-SAI       PIC 9(05).
            05 NOME-SAI         PIC X(20).
            05 DATA-NASC-SAI.
               06 DD-SAI        PIC 9(02).
               06 MM-SAI        PIC 9(02).
               06 AAAA-SAI      PIC 9(04).
            

       WORKING-STORAGE SECTION.

       77  FIM-ARQ          PIC X(03)             VALUE   "NAO".
       PROCEDURE                                   DIVISION.
       
       PGM-EXER02.

	       PERFORM    INICIO.

	       PERFORM    PRINCIPAL 
                    UNTIL   FIM-ARQ      EQUAL     "SIM".
      	 
	       PERFORM   TERMINO.
	  
           STOP RUN.
       INICIO.

           OPEN INPUT       CADALU
               OUTPUT       CADATU.

           PERFORM       LEITURA.      


       LEITURA.

           READ CADALU
                    AT END 
                    MOVE  "SIM"        TO        FIM-ARQ.
       PRINCIPAL.

	       PERFORM             GRAVACAO.
	       PERFORM             LEITURA.


       GRAVACAO.

           MOVE      NUMERO-ENT      TO          NUMERO-SAI.
           MOVE      NOME-ENT        TO          NOME-SAI.
           MOVE      DATA-NASC-ENT   TO          DATA-NASC-SAI.
           WRITE     REG-SAI.


       TERMINO.
           CLOSE       CADALU
                       CADATU.
                      

