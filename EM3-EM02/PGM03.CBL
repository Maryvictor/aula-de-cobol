       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PGM03.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     14/06/2020.
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
           SELECT   TRAB             ASSIGN      TO      DISK.
           SELECT   RELCURA          ASSIGN      TO      DISK.

       DATA                           DIVISION.

       FILE SECTION.
       FD  CADENT
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CADENT1.DAT".
      
       01  REG-ENT.
             02 ESTADO-ENT       PIC X(02).
             02 CIDADE-ENT       PIC X(30).
             02 DATA-ENT.
                03 DIA-ENT       PIC 9(02).
                03 MES-ENT       PIC 9(02).
                03 ANO-ENT       PIC 9(04).
             02 CASOS-ENT        PIC 9(05).
             02 CURADOS-ENT      PIC 9(05).   

       SD  TRAB.
       01  REG-TRAB.
             02 ESTADO-TRAB       PIC X(02).
             02 CIDADE-TRAB       PIC X(30).
             02 DATA-TRAB.
                03 DIA-TRAB       PIC 9(02).
                03 MES-TRAB       PIC 9(02).
                03 ANO-TRAB       PIC 9(04).
             02 CASOS-TRAB        PIC 9(05).
             02 CURADOS-TRAB      PIC 9(05).
           


       FD  RELCURA
           LABEL         RECORD         IS          OMITTED.

       01  REG-REL           PIC X(80).
            

       WORKING-STORAGE SECTION.

       77  FIM-ARQ          PIC X(03)             VALUE   "NAO".
       77  CT-LIN           PIC 9(02)             VALUE  20.
       77  CT-PAG           PIC 9(02)             VALUE  ZEROES.
       77  VAR-CID          PIC X(30).
       77  VAR-EST          PIC X(02).

       01  CAB-01.
           02 FILLER         PIC X(15) VALUE SPACES.
           02 FILLER         PIC X(44)   VALUE 
                         "RELACAO DE PESSOAS CURADAS POR ESTADO/CIDADE".
           02 FILLER         PIC X(11) VALUE SPACES.
           02 FILLER         PIC X(05) VALUE "PAG. ".
           02 VAR-PAG        PIC 9(03).
           02 FILLER         PIC X(02) VALUE SPACES.

       01  CAB-02.
           02 FILLER         PIC X(05) VALUE SPACES.
           02 FILLER         PIC X(08) VALUE "ESTADO: ".
           02 EST            PIC X(02).
           02 FILLER         PIC X(65) VALUE SPACES.

       01  CAB-03.
           02 FILLER         PIC X(10) VALUE SPACES.
           02 FILLER         PIC X(08) VALUE "CIDADE: ".
           02 CID            PIC X(30).
           02 FILLER         PIC X(32) VALUE SPACES.

       01  CAB-04.
           02 FILLER         PIC X(09) VALUE SPACES.
           02 FILLER         PIC X(04) VALUE "DATA". 
           02 FILLER         PIC X(18) VALUE SPACES.
           02 FILLER         PIC X(17) VALUE "CASOS CONFIRMADOS".
           02 FILLER         PIC X(08) VALUE  SPACES.
           02 FILLER         PIC X(15) VALUE "PESSOAS CURADAS". 
           02 FILLER         PIC X(09) VALUE SPACES.       

       01  DETALHE.
           02 FILLER         PIC X(06) VALUE SPACES.
           02 DDD            PIC 9(02).
           02 FILLER         PIC X(01) VALUE "/".
           02 MMM            PIC 9(02).
           02 FILLER         PIC X(01) VALUE "/".
           02 AAA            PIC 9(04).
           02 FILLER         PIC X(20) VALUE SPACES.
           02 CASOS            PIC 9(05).
           02 FILLER         PIC X(20) VALUE SPACES.
           02 CURADOS            PIC 9(05).
           02 FILLER         PIC X(14) VALUE SPACES.

       PROCEDURE                                   DIVISION.
       
           SORT TRAB
                ASCENDING KEY ESTADO-TRAB
                ASCENDING KEY CIDADE-TRAB
                INPUT  PROCEDURE ROT-ENTRADA
                OUTPUT PROCEDURE ROT-SAIDA.
           STOP RUN.     
	       
       ROT-ENTRADA   SECTION.
           PERFORM INICIO-ENTRADA.
           PERFORM PRINCIPAL-ENTRADA
                   UNTIL
                   FIM-ARQ EQUAL "SIM".
           PERFORM FIM-ENTRADA.             

       INICIO-ENTRADA SECTION.
           OPEN INPUT CADENT 
           PERFORM LEITURA-ENTRADA.

       LEITURA-ENTRADA SECTION.
           READ CADENT 
                AT END
                MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL-ENTRADA SECTION.
           PERFORM GRAVACAO-ENTRADA.
           PERFORM LEITURA-ENTRADA.

       GRAVACAO-ENTRADA SECTION.
           MOVE ESTADO-ENT TO ESTADO-TRAB.
           MOVE CIDADE-ENT TO CIDADE-TRAB.
           MOVE DIA-ENT    TO DIA-TRAB.
           MOVE MES-ENT    TO MES-TRAB.
           MOVE ANO-ENT    TO ANO-TRAB.
           MOVE CASOS-ENT  TO CASOS-TRAB.
           MOVE CURADOS-ENT TO CURADOS-TRAB.
           RELEASE REG-TRAB.

       FIM-ENTRADA SECTION.
           CLOSE CADENT.    

                                
       ROT-SAIDA SECTION.
           PERFORM INICIO-SAIDA.
           PERFORM PRINCIPAL-SAIDA
                   UNTIL
                   FIM-ARQ EQUAL "SIM".
           PERFORM FIM-SAIDA.

       INICIO-SAIDA SECTION.
           MOVE "NAO" TO FIM-ARQ.
           OPEN  OUTPUT RELCURA.
           PERFORM LE-SAIDA.  

       LE-SAIDA SECTION.
           RETURN TRAB
                  AT END    
                  MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL-SAIDA SECTION.
           PERFORM IMPRESSAO-SAIDA.
           PERFORM LE-SAIDA.           
                
       IMPRESSAO-SAIDA SECTION.
           IF CT-LIN GREATER THAN 19 
                               OR 
                               (VAR-CID <> CIDADE-TRAB) 
                               OR 
                               (VAR-EST <> ESTADO-TRAB) 
                PERFORM CABECALHO-SAIDA.
           PERFORM DETALHE-SAIDA. 
             
          
       




       DETALHE-SAIDA SECTION.

           MOVE      DIA-TRAB    TO         DDD.
           MOVE      MES-TRAB    TO         MMM.
           MOVE      ANO-TRAB    TO         AAA.
           MOVE      CASOS-TRAB  TO         CASOS.
           MOVE      CURADOS-TRAB TO        CURADOS.
           WRITE     REG-REL FROM DETALHE AFTER ADVANCING 1 LINE.
           ADD       1            TO          CT-LIN.

       CABECALHO-SAIDA SECTION.
           ADD       1            TO    CT-PAG.
           MOVE CT-PAG            TO    VAR-PAG.
           MOVE ESTADO-TRAB       TO    EST.
           MOVE CIDADE-TRAB       TO    CID.
           MOVE CIDADE-TRAB        TO    VAR-CID.
           MOVE ESTADO-TRAB        TO    VAR-EST.
           WRITE REG-REL FROM CAB-01 AFTER ADVANCING 2 LINES.
           WRITE REG-REL FROM CAB-02 AFTER ADVANCING 2 LINES.
           WRITE REG-REL FROM CAB-03 AFTER ADVANCING 2 LINES.
           WRITE REG-REL FROM CAB-04 AFTER ADVANCING 2 LINES.
           MOVE ZEROES TO CT-LIN.
           
        



       FIM-SAIDA SECTION.
           CLOSE       RELCURA.
                      

