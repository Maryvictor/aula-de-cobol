       IDENTIFICATION DIVISION.
       PROGRAM-ID.       LE-IMPRIME.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     01/06/2020.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.    
      *REMARKS.  LE UM ARQUIVO E IMPRIME UM RELATORIO.
       ENVIRONMENT             DIVISION.
       
       CONFIGURATION SECTION.
             SOURCE-COMPUTER. MARIA-PC.
             OBJECT-COMPUTER. MARIA-PC.
             SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT   CAD-USU           ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.
           SELECT   REL-ATR           ASSIGN      TO      DISK.

       DATA                           DIVISION.

       FILE SECTION.
       FD  CAD-USU
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CAD-USU.DAT".
      
       01  REG-USU.
             02 COD-USU       PIC 9(04).
             02 NOM-USU       PIC X(30).
             02 DATA-USU.
                03 DIA        PIC 9(02).
                03 MES        PIC 9(02).
                03 ANO        PIC 9(04).
             
       FD  REL-ATR
           LABEL         RECORD         IS          OMITTED.

       01  REG-ATR           PIC X(80).
            

       WORKING-STORAGE SECTION.

       77  FIM-ARQ          PIC X(03)             VALUE   "NAO".
       77  CT-LIN           PIC 9(02)             VALUE  25.
       77  CT-PAG           PIC 9(02)             VALUE  ZEROES.

       01  CAB-01.
           02 FILLER         PIC X(70) VALUE SPACES.
           02 FILLER         PIC X(05) VALUE "PAG. ".
           02 VAR-PAG        PIC 9(03).
           02 FILLER         PIC X(02) VALUE SPACES.

       01  CAB-02.
           02 FILLER         PIC X(15) VALUE SPACES.
           02 FILLER         PIC X(25) VALUE "RELATORIO DE USUARIOS DA".
           02 FILLER         PIC X(24) VALUE " BIBLIOTECA BOM TEMPO".
           02 FILLER         PIC X(16) VALUE SPACES.

       01  CAB-03.
           02 FILLER         PIC X(5)  VALUE SPACES.
           02 FILLER         PIC X(6) VALUE "CODIGO".
           02 FILLER         PIC X(19) VALUE SPACES.
           02 FILLER         PIC X(04) VALUE "NOME". 
           02 FILLER         PIC X(20) VALUE SPACES.
           02 FILLER         PIC X(21) VALUE "DATA DE CADASTRAMENTO".
           02 FILLER         PIC X(05) VALUE SPACES.

       01  DETALHE.
           02 FILLER         PIC X(06) VALUE SPACES.
           02 COD            PIC 9(04).
           02 FILLER         PIC X(06) VALUE SPACES.  
           02 NOM            PIC X(30).
           02 FILLER         PIC X(13) VALUE SPACES.
           02 DDD            PIC 9(02).
           02 FILLER         PIC X(01) VALUE "/".
           02 MMM            PIC 9(02).
           02 FILLER         PIC X(01) VALUE "/".
           02 AAA            PIC 9(04).
           02 FILLER         PIC X(11) VALUE SPACES.

       PROCEDURE                                   DIVISION.
       
       PGM-EX05.

	       PERFORM    INICIO.

	       PERFORM    PRINCIPAL 
                    UNTIL   FIM-ARQ      EQUAL     "SIM".
      	 
	       PERFORM   FIM.
	  
           STOP RUN.
       INICIO.

           OPEN INPUT       CAD-USU
               OUTPUT       REL-ATR.

           PERFORM       LEITURA.      


       LEITURA.

           READ CAD-USU
                    AT END 
                    MOVE  "SIM"        TO        FIM-ARQ.
       PRINCIPAL.

	       PERFORM             IMPRESSAO.
	       PERFORM             LEITURA.
       
       IMPRESSAO.
           IF CT-LIN GREATER THAN 24
              PERFORM CABECALHO.
           PERFORM IMPDET.   

       IMPDET.

           MOVE      COD-USU      TO          COD.
           MOVE      NOM-USU      TO          NOM.
           MOVE      DIA          TO          DDD.
           MOVE      MES          TO          MMM.
           MOVE      ANO          TO          AAA.
           WRITE     REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE.
           ADD       1            TO          CT-LIN.

       CABECALHO.
           ADD       1            TO    CT-PAG.
           MOVE CT-PAG            TO    VAR-PAG.
           WRITE REG-ATR AFTER ADVANCING PAGE.
           WRITE REG-ATR FROM CAB-01 AFTER ADVANCING 1 LINE.
           WRITE REG-ATR FROM CAB-02 AFTER ADVANCING 2 LINES.
           WRITE REG-ATR FROM CAB-03 AFTER ADVANCING 2 LINES.
           MOVE ZEROES TO CT-LIN.



       FIM.
           CLOSE       CAD-USU
                       REL-ATR.
                      

