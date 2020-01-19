       IDENTIFICATION DIVISION.
       PROGRAM-ID CONSCUR-COB.
      *           SISTEMAS DE COMPUTACAO
      *    ANALISTA         :ENZO 19 - JAMILE 26
      *    PROGRAMADOR(A)   :ENZO 19 - JAMILE 26
      *    DATA             :
      *    FINALIDADE       :CONSULTA DE CURSOS
      *    VRS              DATA              DESCRICAO
      *    1.5              10/10/2000        IMPLANTACAO

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT curcad ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY cod-curso
                         FILE STATUS status-arq.

       DATA DIVISION.

       FILE SECTION.

       FD curcad
           LABEL record STANDARD
           value OF FILE-ID 'curso.dat'
           record contains 52 characteres.

       01 reg-oarq.
           05 cod-curso            PIC 9(06).
           05 descricao            PIC X(40).
           05 mensalidade          PIC 9(04)v99.

       WORKING-STORAGE SECTION.
       01 ws-resp                  PIC x.
       01 linha-branco             PIC X(40) value spaces.
       01 status-arq               PIC X(02) value spaces.

       SCREEN SECTION.
       01  tela-cadastra.
           05 blank screen.
           05  LINE 01 COLUMN 01   VALUE "ษอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออป".
           05  LINE 02 COLUMN 01 VALUE "บ".
           05  LINE 02 COLUMN 80 VALUE "บ".
           05  LINE 03 COLUMN 01 VALUE "บ".
           05  LINE 03 COLUMN 80 VALUE "บ".
           05  LINE 04 COLUMN 01   VALUE "ฬอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออน".
           05  LINE 05 COLUMN 01 VALUE "บ".
           05  LINE 05 COLUMN 80 VALUE "บ".
           05  LINE 06 COLUMN 01 VALUE "บ".
           05  LINE 06 COLUMN 80 VALUE "บ".
           05  LINE 07 COLUMN 01 VALUE "บ".
           05  LINE 07 COLUMN 80 VALUE "บ".
           05  LINE 08 COLUMN 01 VALUE "บ".
           05  LINE 08 COLUMN 80 VALUE "บ".
           05  LINE 09 COLUMN 01 VALUE "บ".
           05  LINE 09 COLUMN 80 VALUE "บ".
           05  LINE 10 COLUMN 01 VALUE "บ".
           05  LINE 10 COLUMN 80 VALUE "บ".
           05  LINE 11 COLUMN 01 VALUE "บ".
           05  LINE 11 COLUMN 80 VALUE "บ".
           05  LINE 12 COLUMN 01 VALUE "บ".
           05  LINE 12 COLUMN 80 VALUE "บ".
           05  LINE 13 COLUMN 01 VALUE "บ".
           05  LINE 13 COLUMN 80 VALUE "บ".
           05  LINE 14 COLUMN 01 VALUE "บ".
           05  LINE 14 COLUMN 80 VALUE "บ".
           05  LINE 15 COLUMN 01 VALUE "บ".
           05  LINE 15 COLUMN 80 VALUE "บ".
           05  LINE 16 COLUMN 01 VALUE "บ".
           05  LINE 16 COLUMN 80 VALUE "บ".
           05  LINE 17 COLUMN 01 VALUE "บ".
           05  LINE 17 COLUMN 80 VALUE "บ".
           05  LINE 18 COLUMN 01 VALUE "บ".
           05  LINE 18 COLUMN 80 VALUE "บ".
           05  LINE 19 COLUMN 01 VALUE "บ".
           05  LINE 19 COLUMN 80 VALUE "บ".
           05  LINE 20 COLUMN 01 VALUE "บ".
           05  LINE 20 COLUMN 80 VALUE "บ".
           05  LINE 21 COLUMN 01 VALUE "บ".
           05  LINE 21 COLUMN 80 VALUE "บ".
           05  LINE 22 COLUMN 01 VALUE "บ".
           05  LINE 22 COLUMN 80 VALUE "บ".
           05  LINE 23 COLUMN 01 VALUE "บ".
           05  LINE 23 COLUMN 80 VALUE "บ".
            
           05  LINE 24 COLUMN 01   VALUE "ศอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออผ". 
           05 line  02   column 02  value 'MENU'.
           05 line  02   column 31  value 'CONSULTA DE CURSOS'.
           05 line  02   column 72  value 'VRS 1.5'.
           05 line  06   column 07  value 'CODIGO CURSO [000000]'.
           05 line  08   column 07  value 'DESCRICAO ['.
           05 line  08   column 58  value ']'.
           05 line  10   column 07  value 'MENSALIDADE'.
           05 line  10   column 19  value '[       ]'.

       PROCEDURE DIVISION.
       0100-open.
           OPEN I-O curcad
           IF status-arq = '00'
              GO TO 0200-tela.
           DISPLAY (12 30) '[CURSO.DAT INEXISTENTE]'
           STOP RUN.

       0200-tela.
           DISPLAY tela-cadastra.

       0210-cod-curso.
           ACCEPT(06 21) cod-curso with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco.
           IF cod-curso = ZEROS
                       GO TO 1300-finaliza.
           READ curcad INVALID KEY
                DISPLAY (22 20) 'cliente inexistente para consulta'
                GO TO 0210-cod-curso.

           DISPLAY (08 18) descricao
           DISPLAY (10 20) mensalidade.
           

       1300-finaliza.
           DISPLAY (21 20) 'DESEJA CONTINUAR A CONSULTA ? S/N [ ]'.

       1400-resp.
           ACCEPT (21 55) ws-resp with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco.
           IF ws-resp = 'S' or 's'
              GO TO 0200-tela.
           IF ws-resp = 'N' or 'n'
              CLOSE curcad
              CHAIN 'FACAD.EXE '
           ELSE
              DISPLAY (22 20) 'RESPOSTA INVALIDA'
              GO TO 1400-resp.







