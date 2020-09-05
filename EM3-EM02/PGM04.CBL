       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PGM04.
       AUTHOR.           MARIA VICTOR.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     09/06/2020.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.    
      *REMARKS.  LE OS REGISTRO DO ARQUIVO DE
      *          ENTRADA CADENT, CALCULAR O SALARIO E GRAVAR NO
      *          ARQUIVO DE SAIDA CADSAI POR ORDEM ASCENDENTE POR NOME.
      
       ENVIRONMENT             DIVISION.
       
       CONFIGURATION SECTION.
             SOURCE-COMPUTER. MARIA-PC.
             OBJECT-COMPUTER. MARIA-PC.
             SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT   CADENT       ASSIGN      TO      DISK
           ORGANIZATION   IS           LINE         SEQUENTIAL.
           SELECT   TRAB          ASSIGN      TO      DISK.

           SELECT   CADSAI           ASSIGN      TO      DISK
           ORGANIZATION    IS         LINE          SEQUENTIAL.
       DATA                           DIVISION.

       FILE SECTION.
       FD  CADENT
           LABEL         RECORD          ARE     STANDARD
           VALUE        OF FILE-ID       IS      "CADENT.DAT".
      
       01  REG-ENT.
           02 NOME-ENT      PIC X(30).
           02 HORAS-ENT     PIC 9(03).
           02 VALORH-ENT    PIC 9(02)V99.
           02 DESPESAS-ENT  PIC 9(03)V99.
           02 IMPOSTOS-ENT  PIC 9(03)V99.

       SD  TRAB.
       01  REG-TRA.
           02 NOME-TRAB       PIC X(30).
           02 HORAS-TRAB      PIC 9(03).
           02 VALORH-TRAB     PIC 9(02)V99.
           02 DESPESAS-TRAB   PIC 9(03)V99.
           02 IMPOSTOS-TRAB   PIC 9(03)V99.
                
             
       FD  CADSAI
           LABEL         RECORD         ARE        STANDARD
           VALUE        OF FILE-ID      IS       "CADSAI.DAT".
       
       01   REG-SAI.
            03 NOME-SAI         PIC X(30).
            03 SALARIOL-SAI     PIC 9(05)V99.
            

       WORKING-STORAGE SECTION.

       77     FIM-ARQ         PIC X(03) VALUE  "NAO".
       77     WR-SALARIO      PIC 9(05)V99 VALUE ZERO.
       77     VAR-SALARIO     PIC 9(05)V99 VALUE ZERO.
       

       PROCEDURE                                   DIVISION.
       
       PGM-EXE04.

	       SORT    TRAB
                   ASCENDING KEY NOME-TRAB
                   USING CADENT
                   OUTPUT PROCEDURE ROT-SAIDA
            STOP RUN.   

       ROT-SAIDA              SECTION.
           PERFORM   INICIO.
           PERFORM   PRINCIPAL
                    UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM FIM.


       INICIO                 SECTION.
           OPEN    OUTPUT CADSAI.
           PERFORM LE-SORT.


       LE-SORT                SECTION.
           RETURN  TRAB
                   AT END
                   MOVE "SIM"  TO   FIM-ARQ.


       PRINCIPAL              SECTION.
            PERFORM  CALCULO.
            PERFORM GRAVACAO.
            PERFORM  LE-SORT.
            


       CALCULO                 SECTION.
           MULTIPLY HORAS-TRAB BY VALORH-TRAB GIVING WR-SALARIO.
           SUBTRACT DESPESAS-TRAB IMPOSTOS-TRAB FROM WR-SALARIO GIVING 
                                                VAR-SALARIO. 
                                      
           

       GRAVACAO                SECTION.
           MOVE NOME-TRAB   TO  NOME-SAI.
           MOVE VAR-SALARIO TO  SALARIOL-SAI.
           WRITE REG-SAI.


       FIM                      SECTION.
           CLOSE  CADSAI.                                

