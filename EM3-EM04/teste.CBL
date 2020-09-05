       IDENTIFICATION DIVISION.
       PROGRAM-ID.       LE-IMPRIME.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     16/06/2020.
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
           SELECT   TRAB              ASSIGN      TO      DISK.
           SELECT   RELTRAN             ASSIGN      TO      DISK.

       DATA                           DIVISION.

       FILE SECTION.
       FD  CADENT
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CADENT.DAT".
      
       01  REG-ENT.
             02 NOME-ENT         PIC X(30).
             02 DEPARTAMENTO-ENT PIC 9(02).
             02 PREC-TRANS-ENT   PIC 9(4)V99.
             02 DIAS-TRAB-ENT    PIC 99.

       SD  TRAB.
       01  REG-TRAB.
             02 NOME-TRAB         PIC X(30).
             02 DEPARTAMENTO-TRAB PIC 9(02).
             02 PREC-TRANS-TRAB   PIC 9(4)V99.
             02 DIAS-TRAB-TRAB    PIC 99.

             
             
       FD  RELTRAN 
           LABEL         RECORD         IS          OMITTED.

       01  REG-REL           PIC X(80).
            

       WORKING-STORAGE SECTION.

       77  FIM-ARQ          PIC X(03)             VALUE   "NAO".
       77  CT-LIN           PIC 9(02)             VALUE  30.
       77  CT-PAG           PIC 9(02)             VALUE  ZEROES.

       01  CAB-01.
           02 FILLER         PIC X(10) VALUE SPACES.
           02 FILLER         PIC X(37) VALUE 
                            "RELACAO DE VALORES DO TRANSPORTE PAGO".
           02 FILLER         PIC X(23) VALUE SPACES.                
           02 FILLER         PIC X(05) VALUE "PAG. ".
           02 VAR-PAG        PIC 9(02).
           02 FILLER         PIC X(03) VALUE SPACES.

       01  CAB-02.
           02 FILLER         PIC X(30) VALUE SPACES.
           02 FILLER         PIC X(30) VALUE 
                             "PARA TRABALHADOR DO DPTO DE TI".
           02 FILLER         PIC X(15) VALUE SPACES.
           02 FILLER         PIC X(25) VALUE SPACES.

       01  CAB-03. 
           02 FILLER         PIC X(05) VALUE SPACES.
           02 FILLER         PIC X(04) VALUE "NOME".
           02 FILLER         PIC X(30) VALUE SPACES.
           02 FILLER         PIC X(10) VALUE "VALOR PAGO.
.        

       01  DETALHE.
           02 FILLER         PIC X(05) VALUE SPACES.
           02 NOME-DET       PIC 9(30).
           02 FILLER         PIC X(13) VALUE SPACES.  
           02 VALOR-DET      PIC 9(06).
           02 FILLER         PIC X(02) VALUE SPACES.
        

       PROCEDURE                                   DIVISION.
       
       PGM-EX.
      
           SORT TRAB
                ASCENDING KEY NOME-TRAB
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
           PERFORM SELECAO-ENTRADA.
           PERFORM CALCULO-ENTRADA.
           PERFORM LEITURA-ENTRADA.

       SELECAO-ENTRADA SECTION.
           IF DEPARTAMENTO-ENT EQUAL 32
               PERFORM GRAVACAO-ENTRADA.

       CALCULO-ENTRADA SECTION.
           MULTIPLY PREC-TRANS-ENT  BY 2 GIVING VR-M.
           MULTIPLY VR-M  BY DIAS-TRAB-ENT GIVING VALOR-VR.      

       GRAVACAO-ENTRADA SECTION.
           MOVE NOME-ENT  TO NOME-TRAB.
           MOVE VALOR-VR  TO VR-VALOR.
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
           OPEN  OUTPUT RELTRAN.
           PERFORM LE-SAIDA.  

       LE-SAIDA SECTION.
           RETURN TRAB
                  AT END    
                  MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL-SAIDA SECTION.
           PERFORM IMPRESSAO-SAIDA.
           PERFORM LE-SAIDA.           
                
       IMPRESSAO-SAIDA SECTION.
           IF CT-LIN GREATER THAN 29
              PERFORM CABECALHO-SAIDA.
           PERFORM DETALHE-SAIDA.   

       DETALHE-SAIDA SECTION.

           
           MOVE      NOME-TRAB    TO         NOME-DET.
           MOVE      VR-VALOR     TO         VALOR-DET.
           WRITE     REG-REL FROM DETALHE AFTER ADVANCING 1 LINE.
           ADD       1            TO          CT-LIN.

       CABECALHO-SAIDA SECTION.
           ADD       1            TO    CT-PAG.
           MOVE CT-PAG            TO    VAR-PAG.
           WRITE REG-REL AFTER ADVANCING PAGE.
           WRITE REG-REL FROM CAB-01 AFTER ADVANCING 1 LINE.
           WRITE REG-REL FROM CAB-02 AFTER ADVANCING 2 LINES.
           WRITE REG-REL FROM CAB-03 AFTER ADVANCING 2 LINES.
           MOVE ZEROES TO CT-LIN.



       FIM-SAIDA SECTION.
           CLOSE       RELTRAN.
                      

