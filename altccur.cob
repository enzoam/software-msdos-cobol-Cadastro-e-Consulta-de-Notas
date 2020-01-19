       IDENTIFICATION DIVISION.
       PROGRAM-ID ALTCUR-COB.
      *           SISTEMAS DE COMPUTACAO
      *    ANALISTA         :ENZO E JAMILE
      *    PROGRAMADOR(A)   :ENZO E JAMILE
      *    DATA             :
      *    FINALIDADE       :ALTERACAO DE CURSOS
      *    VRS              DATA              DESCRICAO
      *    1.5                /  /            IMPLANTACAO

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
       01 ws-retorno               PIC X(02). 
           88 ESC                  VALUE '01'.
       SCREEN SECTION.
       01 tela-cadastra.
           05 BLANK SCREEN.
           05 line  02   column 02  value 'MENU'.
           05 line  02   column 31  value 'ALTERACAO DE CURSOS'.
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
           DISPLAY (12 30) '[CURSO.DAT INEXISTENTE - ABORTADO]'
           GO TO 0100-open.

       0200-tela.
           DISPLAY tela-cadastra.

       0210-cod-curso.
           ACCEPT(06 21) cod-curso with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco.
           IF cod-curso = ZEROS
              GO TO 1300-finaliza.
           READ curcad INVALID KEY
                   DISPLAY (23 20) 'cliente inexistente para alteracao'
                   GO TO 0210-cod-curso.	
           
       0300-desc-curso.
           ACCEPT(08 18) descricao with update AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (08 18) '                      '
              GO TO 0210-cod-curso.    
           IF descricao = spaces
              DISPLAY (22 20) 'NOME INVALIDO'
              GO TO 0300-desc-curso.

       0400-mensalidade.
           ACCEPT(10 20) mensalidade with update AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (10 20) '      '
              GO TO 0300-desc-curso.    
		IF mensalidade = ZEROS
              DISPLAY (22 20) 'MENSALIDADE INVALIDA...'
              GO TO 0400-mensalidade.

       1200-confirma.
           DISPLAY (18 20) 'CONFIRMA OS DADOS ACIMA ? S/N [ ]'.
           ACCEPT (18 51) ws-resp with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco.
           IF ws-resp='S' or 's'
              REWRITE reg-oarq
              IF status-arq = '24' or '34'
                 DISPLAY (22 20) 'ARQUIVO CHEIO - ABORTADO...'
                 CLOSE curcad
                 STOP RUN
              ELSE
                 GO TO 1300-finaliza.
           IF ws-resp='N' or 'n'
              GO TO 1300-finaliza.
           DISPLAY (22 20) 'RESPOSTA INVALIDA'
           GO TO 1200-confirma.

       1300-finaliza.
           DISPLAY (21 20) 'DESEJA CONTINUAR ALTERACAO ? S/N [  ]'.

       1400-resp.
           ACCEPT (21 55) ws-resp with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco.
           IF ws-resp = 'S' or 's'
              GO TO 0200-tela.
           IF ws-resp = 'N' or 'n'
              CLOSE curcad
              STOP RUN.
           DISPLAY (22 20) 'RESPOSTA INVALIDA'
           GO TO 1400-resp.







