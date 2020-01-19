       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLASSIF-COB.
      *               EMPRESA S/A
      *    ANALISTA         :JORGE KOIKE
      *    PROGRAMADO(A)    :ENZO/JAMILE
      *    DATA             :03/08/00
      *    FINALIDADE       :EMITE RELACAO ALFABETICA DO CADASTRO
      *    VRS              DATA              DESCRICAO
      *    1.0              23/02/00          Implantacao

       ENVIRONMENT DIVISION.    
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CADASTRO ASSIGN TO DISK
                           ORGANIZATION INDEXED
                           ACCESS MODE DYNAMIC
                           RECORD KEY CHAVE-ARQ
                           FILE STATUS STATUS-CAD.

           SELECT OWORK ASSIGN TO DISK
                           ORGANIZATION INDEXED
                           ACCESS MODE DYNAMIC
                           RECORD KEY CHAVE-OWORK
                           FILE STATUS STATUS-OWORK.

       DATA DIVISION.

       FILE SECTION.
       FD CADASTRO
           LABEL record STANDARD
           value OF FILE-ID 'cadastro.dat'
           record contains 96 characteres.
       01  REG-ARQ.
           05 CHAVE-ARQ.
              10 cod-loja            PIC 9(04).
              10 contrato            PIC 9(09).
           05 nome                   PIC X(40).
           05 endereco               PIC X(30).
           05 vencimento.
              10 ano-venc            PIC 9(04).
              10 mes-venc            PIC 9(02).
              10 dia-venc            PIC 9(02).
           05 vencimento-R Redefines vencimento
                                     PIC 9(08).
           05 prestacao              PIC 9(07)V99 comp-3.
           05 prestacao-R Redefines prestacao
                                     PIC 9(09)    comp-3.

       FD OWORK
           LABEL record STANDARD
           value OF FILE-ID 'owork.dat'
           record contains 96 characteres.
       01  REG-OWORK.
           05 CHAVE-OWORK.
              10 nome-owork          PIC X(40).
              10 loja-owork          PIC 9(04).
              10 contrato-owork      PIC 9(09).
           05 FILLER                 PIC X(43).

       WORKING-STORAGE SECTION.
       01  status-cad              PIC X(02).
       01  status-owork            PIC X(02).

       PROCEDURE DIVISION.

       0100-open.
           OPEN INPUT CADASTRO
           IF STATUS-CAD = '30'
              CLOSE CADASTRO
              STOP RUN.
           OPEN OUTPUT OWORK
           CLOSE OWORK
           OPEN I-O OWORK.

       0200-LE-CAD.
           READ CADASTRO NEXT
           IF STATUS-OWORK = '10'
              CLOSE CADASTRO
                    OWORK
              OPEN INPUT OWORK
              GO TO 0300-LE-OWORK.
           IF STATUS-OWORK NOT = '00'
              DISPLAY (23 20) 'Problema Read'
              STOP RUN.
           MOVE COD-LOJA TO LOJA-OWORK
           MOVE NOME     TO NOME-OWORK
           MOVE CONTRATO TO CONTRATO-OWORK
           READ OWORK INVALID KEY
                      WRITE REG-OWORK FROM REG-ARQ
                      GO TO 0200-LE-CAD.
           DISPLAY(23 30) ' ISTRO DUPLICADO'
           CLOSE CADASTRO
                 OWORK
           STOP RUN.

       0300-LE-OWORK.
           READ OWORK
           IF STATUS-OWORK = '10'
              CLOSE OWORK
              STOP RUN.

           IF STATUS-OWORK NOT = '00'
              DISPLAY(23 20) 'PROBL READ OWORK'
              CLOSE OWORK
              STOP RUN.

           DISPLAY(1 1) ERASE
           DISPLAY(12 20) NOME-OWORK
           STOP '  '
           GO TO 0300-LE-OWORK.

