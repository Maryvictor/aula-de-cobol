       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PGM05.
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

           SELECT   CADENT           ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.
           SELECT   REL-SAI          ASSIGN      TO      DISK.

       DATA                           DIVISION.

       FILE SECTION.
       FD  CADENT
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CADENT.DAT".
      
       01  REG-USU.
             02 CPF-USU.
                03 NNN-U        PIC 9(03).
                03 YYY-U        PIC 9(03).
                03 HHH-U        PIC 9(03).
                02 RR-U         PIC 9(02).
             02 NOM-USU       PIC X(30).
             02 ESTADO-USU    PIC X(02).
             02 CIDADE-USU    PIC X(30).
             02 TELEFONE-USU  PIC 9(08).
             02 DATA-USU.
                03 DIA        PIC 9(02).
                03 MES-USU    PIC 9(02).
                03 ANO        PIC 9(04).
             
       FD  REL-SAI
           LABEL         RECORD         IS          OMITTED.

       01  REG-SAI           PIC X(80).
            

       WORKING-STORAGE SECTION.

       77  FIM-ARQ          PIC X(03)             VALUE   "NAO".
       77  CT-LIN           PIC 9(02)             VALUE  40.
       77  CT-PAG           PIC 9(02)             VALUE  ZEROES.
       

       01  TABELA-MES.
           02 DEF-MES.
              03 FILLER     PIC X(009) VALUE "JANEIRO".
              03 FILLER     PIC X(009) VALUE "FEVEREIRO".
              03 FILLER     PIC X(009) VALUE "MARCO".
              03 FILLER     PIC X(009) VALUE "ABRIL".
              03 FILLER     PIC X(009) VALUE "MAIO".
              03 FILLER     PIC X(009) VALUE "JUNHO".
              03 FILLER     PIC X(009) VALUE "JULHO".
              03 FILLER     PIC X(009) VALUE "AGOSTO".
              03 FILLER     PIC X(009) VALUE "SETEMBRO".
              03 FILLER     PIC X(009) VALUE "OUTUBRO".
              03 FILLER     PIC X(009) VALUE "NOVEMBRO".
              03 FILLER     PIC X(009) VALUE "DEZEMBRO".

       01  TABELA-MES-RED   REDEFINES  TABELA-MES.
            02 MES          PIC X(009)
                            OCCURS 12 TIMES.        
                   
       01  CAB-01.
           02 FILLER         PIC X(70) VALUE SPACES.
           02 FILLER         PIC X(05) VALUE "PAG. ".
           02 VAR-PAG        PIC 9(03).
           02 FILLER         PIC X(02) VALUE SPACES.

       01  CAB-02.
           02 FILLER         PIC X(30) VALUE SPACES.
           02 FILLER         PIC X(17) VALUE "TURISMAR TURISMO".
           02 FILLER         PIC X(33) VALUE SPACES.

       01  CAB-03.
           02 FILLER         PIC X(20) VALUE SPACES.
           02 FILLER         PIC X(34) VALUE 
                               "CLIENTES NO PERIODO DE 2010 A 2011".
           02 FILLER         PIC X(26) VALUE SPACES.

       01  CAB-04.
           02 FILLER        PIC X(15) VALUE SPACES.
           02 FILLER        PIC X(04) VALUE "NOME".
           02 FILLER        PIC X(22) VALUE SPACES.
           02 FILLER        PIC X(03) VALUE "CPF".
           02 FILLER        PIC X(07) VALUE SPACES.
           02 FILLER        PIC X(21) VALUE "DATA DA ULTIMA VISITA".
           02 FILLER        PIC X(08) VALUE SPACES.

       01  CAB-05.
           02 FILLER        PIC X(58) VALUE SPACES.
           02 FILLER        PIC X(14) VALUE "DIA / MES/ ANO".
           02 FILLER        PIC X(08) VALUE SPACES.


       01  DETALHE.
           02 FILLER         PIC X(05) VALUE SPACES.
           02 NOM            PIC X(30).
           02 FILLER         PIC X(03) VALUE SPACES.
           02 NNN            PIC 9(03).
           02 FILLER         PIC X(01) VALUE ".". 
           02 YYY            PIC 9(03).
           02 FILLER         PIC X(01) VALUE ".".
           02 HHH            PIC 9(03).
           02 FILLER         PIC X(01) VALUE "/".
           02 RR             PIC 9(02).
           02 FILLER         PIC X(03) VALUE SPACES.
           02 DDD            PIC 9(02).
           02 FILLER         PIC X(01) VALUE "/".
           02 MMM            PIC X(09).
           02 FILLER         PIC X(01) VALUE "/".
           02 AAA            PIC 9(04).
           02 FILLER         PIC X(08) VALUE SPACES.

       01  CAB-06.
           02 FILLER         PIC X(05) VALUE SPACES.
           02 FILLER         PIC X(19) VALUE "TOTAL DE CLIENTES: ".
           

       
            

       PROCEDURE                                   DIVISION.
       
       PGM-EX05.

	       PERFORM    INICIO.

	       PERFORM    PRINCIPAL 
                    UNTIL   FIM-ARQ      EQUAL     "SIM".
      	 
	       PERFORM   FIM.
	  
           STOP RUN.
       INICIO.

           OPEN INPUT       CADENT
               OUTPUT       REL-SAI.

           PERFORM       LEITURA.      


       LEITURA.

           READ CADENT
                    AT END 
                    MOVE  "SIM"        TO        FIM-ARQ.
       PRINCIPAL. 
        
           PERFORM             PESQUISA-TABELA.
           PERFORM             SELECAO.
	       PERFORM             LEITURA.
       
       PESQUISA-TABELA.
           MOVE  MES (MES-USU) TO MMM.
      


       SELECAO.
           IF ANO EQUAL 2010 OR ANO EQUAL 2011
              PERFORM IMPRESSAO.
    

       IMPRESSAO.
           IF CT-LIN GREATER THAN 38
              PERFORM CABECALHO.
           PERFORM IMPDET.   

       IMPDET.

           MOVE      NOM-USU      TO          NOM.
           MOVE      NNN-U        TO          NNN.
           MOVE      YYY-U        TO          YYY.
           MOVE      HHH-U        TO          HHH.
           MOVE      RR-U         TO          RR.
           MOVE      DIA          TO          DDD.
           MOVE      ANO          TO          AAA.
           WRITE     REG-SAI FROM DETALHE BEFORE ADVANCING 1 LINE.
           ADD       1            TO          CT-LIN.



       CABECALHO.
           ADD       1            TO    CT-PAG.
           MOVE CT-PAG            TO    VAR-PAG.
           WRITE REG-SAI AFTER ADVANCING PAGE.
           WRITE REG-SAI FROM CAB-01 BEFORE ADVANCING 1 LINE.
           WRITE REG-SAI FROM CAB-02 BEFORE ADVANCING 2 LINES.
           WRITE REG-SAI FROM CAB-03 BEFORE ADVANCING 2 LINES.
           WRITE REG-SAI FROM CAB-04 BEFORE ADVANCING 1 LINE.
           WRITE REG-SAI FROM CAB-05 BEFORE ADVANCING 2 LINES.
           MOVE ZEROES TO CT-LIN.



       FIM.
           CLOSE       CADENT
                       REL-SAI.
                      

