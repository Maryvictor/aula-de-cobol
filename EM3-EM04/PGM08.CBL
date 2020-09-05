       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PGM08.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     20/06/2020.
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

           SELECT   CADALU           ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.
           SELECT   TRAB             ASSIGN      TO      DISK.
           SELECT   RELALU           ASSIGN      TO      DISK.

       DATA                           DIVISION.

       FILE SECTION.
       FD  CADALU
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CADALU.DAT".
      
       01  REG-ENT.
             02 COD-ENT         PIC 9(02).
             02 CURSO-ENT       PIC X(30).
             02 DISCIPLINA-ENT  PIC X(30).
             02 MATRICULA.
                03 MATRI-ENT    PIC X(08).
                03 MATRIC-ENT   PIC X(01). 
            
             02 NOME-ENT        PIC X(30).
             02 EMAIL-ENT       PIC X(20).

       SD  TRAB.
       01  REG-TRAB.
             02 COD-TRAB         PIC 9(02).
             02 CURSO-TRAB       PIC X(30).
             02 DISCIPLINA-TRAB  PIC X(30).
             02 MATRICULA-TRAB.
                03 MATRI-TRAB    PIC X(08).
                03 MATRIC-TRAB   PIC X(01). 
             02 NOME-TRAB        PIC X(30).
             02 EMAIL-TRAB       PIC X(20).

    
             
       FD  RELALU 
           LABEL         RECORD         IS          OMITTED.

       01  REG-REL           PIC X(80).
            

       WORKING-STORAGE SECTION.

       77  FIM-ARQ          PIC X(03)             VALUE   "NAO".
       77  CT-LIN           PIC 9(02)             VALUE  40.
       77  CT-PAG           PIC 9(02)             VALUE  ZEROES.
       77  WR-CURSO         PIC X(30).
       77  WR-DISCIPLINA      PIC X(30).

       01  CAB-01.
           02 FILLER         PIC X(20) VALUE SPACES.
           02 FILLER         PIC X(27) VALUE 
                             "LISTA DE ALUNOS DA FATEC-SP".
           02 FILLER         PIC X(23) VALUE SPACES.
           02 FILLER         PIC X(05) VALUE "PAG. ".
           02 VAR-PAG        PIC ZZ9.
           02 FILLER         PIC X(03) VALUE SPACES.

       01  CAB-02.
           02 FILLER         PIC X(03) VALUE SPACES.
           02 FILLER         PIC X(07) VALUE "CURSO: ".
           02 CURSO          PIC X(30).
           02 FILLER         PIC X(40) VALUE SPACES.

       01  CAB-03.
           02 FILLER         PIC X(10) VALUE SPACES.
           02 FILLER         PIC X(12) VALUE "DISCIPLINA: ".
           02 DISCIPLINA     PIC X(30).
           02 FILLER         PIC X(28) VALUE SPACES.


       01  CAB-04.
           02 FILLER         PIC X(03) VALUE SPACES.
           02 FILLER         PIC X(09) VALUE "MATRICULA".
           02 FILLER         PIC X(20) VALUE SPACES.
           02 FILLER         PIC X(04) VALUE "NOME".
           02 FILLER         PIC X(20) VALUE SPACES.
           02 FILLER         PIC X(05) VALUE "EMAIL".
           02 FILLER         PIC X(19) VALUE SPACES.


       01  DETALHE.
           02 FILLER         PIC X(03) VALUE SPACES.
           02 MATRI-DET      PIC 9(08).
           02 FILLER         PIC X(01) VALUE "-".
           02 MATRIC-DET     PIC 9(01).
           02 FILLER         PIC X(02) VALUE SPACES.  
           02 NOME-DET       PIC X(30).
           02 FILLER         PIC X(03) VALUE SPACES.
           02 EMAIL-DET      PIC X(30).
           02 FILLER         PIC X(02) VALUE SPACES.

       PROCEDURE                                   DIVISION.
       
       PGM-EX.
      
           SORT TRAB
                ASCENDING KEY CURSO-TRAB
                ASCENDING KEY DISCIPLINA-TRAB
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
           OPEN INPUT CADALU 
           PERFORM LEITURA-ENTRADA.

       LEITURA-ENTRADA SECTION.
           READ CADALU 
                AT END
                MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL-ENTRADA SECTION.
           PERFORM SELECAO-ENTRADA.
           PERFORM LEITURA-ENTRADA.

       SELECAO-ENTRADA SECTION.
           IF COD-ENT EQUAL 02
               PERFORM GRAVACAO-ENTRADA.

       GRAVACAO-ENTRADA SECTION.
           MOVE COD-ENT        TO COD-TRAB.
           MOVE CURSO-ENT      TO CURSO-TRAB.
           MOVE DISCIPLINA-ENT TO DISCIPLINA-TRAB.
           MOVE MATRI-ENT      TO MATRI-TRAB.
           MOVE MATRIC-ENT     TO MATRIC-TRAB.
           MOVE NOME-ENT       TO NOME-TRAB.
           MOVE EMAIL-ENT      TO EMAIL-TRAB.
           RELEASE REG-TRAB.

       FIM-ENTRADA SECTION.
           CLOSE CADALU.    

                                
       ROT-SAIDA SECTION.
           PERFORM INICIO-SAIDA.
           PERFORM PRINCIPAL-SAIDA
                   UNTIL
                   FIM-ARQ EQUAL "SIM".
           PERFORM FIM-SAIDA.

       INICIO-SAIDA SECTION.
           MOVE "NAO" TO FIM-ARQ.
           OPEN  OUTPUT RELALU.
           PERFORM LE-SAIDA.  

       LE-SAIDA SECTION.
           RETURN TRAB
                  AT END    
                  MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL-SAIDA SECTION.
           PERFORM IMPRESSAO-SAIDA.
           PERFORM LE-SAIDA.           
                
       IMPRESSAO-SAIDA SECTION.
           IF CT-LIN GREATER THAN 39 
                          OR 
              (WR-CURSO <> CURSO-TRAB) 
                          OR
              (WR-DISCIPLINA <> DISCIPLINA-TRAB)
               PERFORM CABECALHO-SAIDA.
           PERFORM DETALHE-SAIDA.   

       DETALHE-SAIDA SECTION.

    
           MOVE      MATRI-TRAB   TO         MATRI-DET
           MOVE      MATRIC-TRAB  TO         MATRIC-DET.
           MOVE      NOME-TRAB    TO         NOME-DET.
           MOVE      EMAIL-TRAB   TO         EMAIL-DET.
           WRITE     REG-REL FROM DETALHE AFTER ADVANCING 1 LINE.
           ADD       1            TO          CT-LIN.

       CABECALHO-SAIDA SECTION.
           ADD       1            TO    CT-PAG.
           MOVE CT-PAG            TO    VAR-PAG.
           WRITE REG-REL FROM CAB-01 AFTER ADVANCING 2 LINE.
           MOVE  CURSO-TRAB       TO    CURSO.
           MOVE  DISCIPLINA-TRAB  TO    DISCIPLINA.
           MOVE  CURSO-TRAB       TO    WR-CURSO.
           MOVE  DISCIPLINA-TRAB  TO    WR-DISCIPLINA.
           WRITE REG-REL FROM CAB-02 AFTER ADVANCING 3 LINES.
           WRITE REG-REL FROM CAB-03 AFTER ADVANCING 2 LINES.
           WRITE REG-REL FROM CAB-04 AFTER ADVANCING 3 LINES.
           MOVE ZEROES TO CT-LIN.



       FIM-SAIDA SECTION.
           CLOSE       RELALU.
                      

