       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENTNOTAS-COB.
      * ANALISTA       : ENZO 19 - JAMILE 26
      * PROGRAMADOR(A) : ENZO 19 - JAMILE 26
      * FINALIDADE : ENTRADA DE NOTAS
      * DATA :
      * VRS         DATA           DESCRICAO
      * 1.5         13/11/2000     IMPLATACAO

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT CADCUR ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-cur
                         FILE STATUS statcur-arq.

           SELECT CADDIS ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-dis
                         FILE STATUS statdis-arq.

           SELECT CADALU ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-alu
                         FILE STATUS statalu-arq.

           SELECT ENTNOT ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-not
                         FILE STATUS statnot-arq.

       DATA DIVISION.
       FILE SECTION.

       FD  CADDIS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'disc.dat'
           RECORD CONTAINS 62 CHARACTERS.

       01  REG-CADDIS.
           05  chave-dis.
               10  cod-dis             PIC 9(06).
               10  dis-cod-cur         PIC 9(06).
               10  ano-dis             PIC 9(04).
           05  desc-dis                PIC X(40).
           05  qtd-aulas-dis           PIC 9(03).
           05  carga-h                 PIC 9(03).

       FD  CADCUR
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'curso.dat'
           RECORD CONTAINS 52 CHARACTERS.

       01  REG-CADCUR.
           05 chave-cur.
              10 cod-curso         PIC 9(06).
           05 desc-cur             PIC X(40).
           05 mensalidade          PIC 9(04)v99.


       FD  CADALU
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'alunos.dat'
           RECORD CONTAINS 135 CHARACTERS.

       01  REG-CADALUNO.
           05  chave-alu.
               10  cod-aluno           PIC 9(06).
               10  alu-cod-cur         PIC 9(06).
           05  nome-alu                PIC X(40).
           05  endereco                PIC X(30).
           05  bairro                  PIC X(20).
           05  cidade                  PIC X(15).
           05  estado                  PIC X(02).
           05  data-nasc.
               10 ano-nasc             PIC 9(04).
               10 mes-nasc             PIC 9(02).
               10 dia-nasc             PIC 9(02).
           05 data-nasc-r REDEFINES data-nasc PIC 9(08).
           05 rg.
               10 rg-num               PIC 9(07).
               10 rg-dig               PIC 9(01).
           05 rg-r REDEFINES rg        PIC 9(08).

       FD  ENTNOT
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'notas.dat'
           RECORD CONTAINS 68 CHARACTERS.

       01  REG-ENTNOT.
           05  chave-not.
               10 ano-nota              PIC 9(04).
               10 nota-cod-alu          PIC 9(06).
               10 nota-cod-cur          PIC 9(06).
               10 nota-cod-dis          PIC 9(06).
           05  nota-bim                 PIC 9.
           05  nota                     PIC 9(02)V9.
           05  falta                    PIC 9(02).
           05  nota-nome-alu            PIC X(40).

       WORKING-STORAGE SECTION.
       01  statcur-arq                  PIC X(02) VALUE SPACES.
       01  statdis-arq                  PIC X(02) VALUE SPACES.
       01  statalu-arq                  PIC X(02) VALUE SPACES.
       01  statnot-arq                  PIC X(02) VALUE SPACES.
       01  linha-branco                 PIC X(45) VALUE SPACES.
       01  ws-bim                       PIC 9.
       01  ws-nota                      PIC 9(02)V9.
       01  ws-faltas                    PIC 9(02).
       01  ws-resp                      PIC X.
       01  ws-voltar                    PIC X.
       01  ws-sair                      PIC 9.
       01  ws-resp-not                  PIC X.
       01  ws-retorno                   PIC X(02).
           88 ESC                       VALUE '01'.

       SCREEN SECTION.
       01  TELA-MAIN.
           05  BLANK SCREEN.
           05  LINE 03 COLUMN 01   VALUE "ษอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออป".
           05  LINE 04 COLUMN 01 VALUE "บ".
           05  LINE 04 COLUMN 80 VALUE "บ".
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
           05  LINE 10 COLUMN 01   VALUE "ศอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออผ".
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
           05  LINE 21 COLUMN 01 VALUE "บ".
           05  LINE 21 COLUMN 80 VALUE "บ".
           05  LINE 20 COLUMN 01   VALUE "ฬอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออน".
           05  LINE 22 COLUMN 01 VALUE "บ".
           05  LINE 22 COLUMN 80 VALUE "บ".
           05  LINE 23 COLUMN 01 VALUE "บ".
           05  LINE 23 COLUMN 80 VALUE "บ".
           05  LINE 24 COLUMN 01   VALUE "ศอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออผ".
           05  LINE 02   COLUMN 02   VALUE "FACAD".
           05  LINE 02   COLUMN 30   VALUE "ENTRADA DE NOTAS".
           05  LINE 02   COLUMN 73   VALUE "VRS 1.5".
           05  LINE 05   COLUMN 04   VALUE "CURSO [......]".
           05  LINE 07   COLUMN 04   VALUE "BIMESTRE [.]".
           05  LINE 07   COLUMN 30   VALUE "ANO [....]".
           05  LINE 09   COLUMN 04   VALUE "COD DISCIPLINA [......]".

       01  TELA-NOTAS.
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
           05  LINE 21 COLUMN 01   VALUE "ฬอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออน".
           05  LINE 22 COLUMN 01 VALUE "บ".
           05  LINE 22 COLUMN 80 VALUE "บ".
           05  LINE 23 COLUMN 01 VALUE "บ".
           05  LINE 23 COLUMN 80 VALUE "บ".
           05  LINE 24 COLUMN 01   VALUE "ศอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออผ".
           05  LINE 11   COLUMN 04   VALUE "COD ALUNO".
           05  LINE 11   COLUMN 17   VALUE "NOME DO ALUNO".
           05  LINE 11   COLUMN 61   VALUE "NOTAS".
           05  LINE 11   COLUMN 69   VALUE "FALTAS".
           05  LINE 13   COLUMN 04   VALUE "[......]".
           05  LINE 13   COLUMN 14   VALUE "[".
           05  LINE 13   COLUMN 54   VALUE "]".
           05  LINE 13   COLUMN 60   VALUE "[....]".
           05  LINE 13   COLUMN 70   VALUE "[..]".
           05  LINE 15   COLUMN 04   VALUE "[......]".
           05  LINE 15   COLUMN 14   VALUE "[".
           05  LINE 15   COLUMN 54   VALUE "]".
           05  LINE 15   COLUMN 60   VALUE "[....]".
           05  LINE 15   COLUMN 70   VALUE "[..]".
           05  LINE 17   COLUMN 04   VALUE "[......]".
           05  LINE 17   COLUMN 14   VALUE "[".
           05  LINE 17   COLUMN 54   VALUE "]".
           05  LINE 17   COLUMN 60   VALUE "[....]".
           05  LINE 17   COLUMN 70   VALUE "[..]".
           05  LINE 19   COLUMN 04   VALUE "[......]".
           05  LINE 19   COLUMN 14   VALUE "[".
           05  LINE 19   COLUMN 54   VALUE "]".
           05  LINE 19   COLUMN 60   VALUE "[....]".
           05  LINE 19   COLUMN 70   VALUE "[..]".


       PROCEDURE DIVISION.

       0010-abre-cadcur.
           OPEN INPUT CADCUR
           IF statcur-arq = '00'
              GO TO 0020-abre-caddis.
           DISPLAY (12 20) 'CURSO.DAT NAO EXISTE'
           STOP RUN.

       0020-abre-caddis.
           OPEN INPUT CADDIS
           IF statdis-arq = '00'
              GO TO 0030-abre-CADALU.
           DISPLAY (12 20) 'DISC.DAT NAO EXISTE'
           CLOSE CADCUR
           STOP RUN.

       0030-abre-cadalu.
           OPEN INPUT CADALU
           IF statalu-arq = '00'
              GO TO 0100-testa-entnot.
           DISPLAY (12 20) 'ALUNOS.DAT NAO EXISTE'
           CLOSE CADCUR
           CLOSE CADDIS
           STOP RUN.

       0100-testa-entnot.
           OPEN I-O ENTNOT
           IF statnot-arq = '00'
              GO TO 0200-tela.
           DISPLAY (12 20)'NOTAS.DAT NAO EXISTE'
           DISPLAY (14 20)'DESEJA GERAR ?(S OU N) [X]'.

       0110-resp.
           ACCEPT (14 44) ws-resp WITH PROMPT AUTO-SKIP
           DISPLAY (17 20) linha-branco
           IF ws-resp = 'S' OR 's'
              OPEN OUTPUT ENTNOT
              CLOSE ENTNOT
              GO TO 0100-testa-entnot.
           IF ws-resp = 'N' OR 'n'
              DISPLAY(17 20) 'ENTRADA DE NOTAS ABORTADO'
              CLOSE CADCUR
              CLOSE CADDIS
              CLOSE CADALU
              STOP RUN.
           DISPLAY(17 20) 'RESPOSTA INVALIDA'
           GO TO 0110-resp.

       0200-tela.
           DISPLAY TELA-MAIN.

       0300-curso.
           ACCEPT (05 11) cod-curso WITH PROMPT AUTO-SKIP
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              GO TO 0200-tela.
           DISPLAY (22 20) linha-branco
           IF COD-CURSO = ZEROS
              GO TO  1200-finaliza.
           READ CADCUR INVALID KEY
                DISPLAY (22 30) 'CURSO NAO CADASTRADO'
                GO TO 0300-curso.
           MOVE cod-curso TO nota-cod-cur
           MOVE cod-curso TO dis-cod-cur
           MOVE cod-curso TO alu-cod-cur
           DISPLAY (05 22) desc-cur.

       0400-bimestre.
           ACCEPT (07 14) ws-bim WITH PROMPT AUTO-SKIP
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              GO TO 0300-curso.
           DISPLAY (22 20) linha-branco
           IF ws-bim < 1 OR > 5
              DISPLAY (22 32) 'BIMESTRE INVALIDO'
              GO TO  0400-bimestre.

       0500-ano.
           ACCEPT (07 35) ano-dis WITH PROMPT AUTO-SKIP
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              GO TO 0400-bimestre.
           DISPLAY (22 20) linha-branco
           MOVE ano-dis TO ano-nota.

       0600-disciplina.
           ACCEPT (09 20) cod-dis WITH PROMPT AUTO-SKIP
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              GO TO 0500-ano.
           DISPLAY (22 20) linha-branco
           IF cod-dis = ZEROS
              DISPLAY (22 33) 'CODIGO INVALIDO'
              GO TO 0600-disciplina.

           READ CADDIS INVALID KEY
                DISPLAY (22 30) 'DISCIPLINA NAO CADASTRADA'
                GO TO 0600-disciplina.

           DISPLAY (09 40) desc-dis
           MOVE cod-dis TO nota-cod-dis.

       0700-notas.
           MOVE 13 TO LIN
           DISPLAY (13 01) ERASE
           DISPLAY TELA-NOTAS.

       0710-testa-bim.
           ACCEPT (LIN , 05) cod-aluno WITH PROMPT AUTO-SKIP
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              GO TO 0600-disciplina.
           DISPLAY (22 20) linha-branco
           IF cod-aluno = ZEROS
              GO TO 1400-volta.
           READ CADALU INVALID KEY
                DISPLAY (22 25) 'ALUNO NAO CADASTRADO'
                GO TO 0710-testa-bim.
           DISPLAY (LIN , 16) nome-alu.
           MOVE nome-alu TO nota-nome-alu
           MOVE cod-aluno TO nota-cod-alu
           READ ENTNOT INVALID KEY
               GO TO 0720-testa-bim1.
           GO TO 0730-testa-outros-bim.


       0720-testa-bim1.
           if ws-bim = 1
              GO TO 0800-nota-cad.
           DISPLAY (22 25) 'NAO CADASTRADO 1งBIM DO ALUNO'
           GO TO 0710-testa-bim.
        

       0730-testa-outros-bim.

           IF ws-bim = 1
                 DISPLAY (22 26) '1ง BIM JA CADASTRADO'
                 GO TO 0710-testa-bim.

           IF ws-bim = 2 AND nota-bim > 1
                 DISPLAY (22 26) '2ง BIM JA CADASTRADO '
                 GO TO 0710-testa-bim.

           IF ws-bim = 2 AND nota-bim = 1
              GO TO 0800-nota-cad.

           IF ws-bim = 3 AND nota-bim > 2
                 DISPLAY (22 26) '3ง BIM JA CADASTRADO '
                 GO TO 0710-testa-bim.

           IF ws-bim = 3 AND nota-bim = 2
                 GO TO 0800-nota-cad.

           IF ws-bim = 4 AND nota-bim > 3
                 DISPLAY (22 26) '4ง BIM JA CADASTRADO '
                 GO TO 0710-testa-bim.

           IF ws-bim = 4 AND nota-bim = 3
                 GO TO 0800-nota-cad.

           IF ws-bim = nota-bim
                 DISPLAY (22 26) 'NOTA FINAL JA CADASTRADA'
                 GO TO 0710-testa-bim.

           IF ws-bim = 5 AND nota-bim < 4
                 DISPLAY (22 26) 'FALTA BIM AINDA NAO CADASTRADO'
                 GO TO 0710-testa-bim.

           IF ws-bim = 5 AND nota-bim = 4
               GO TO 0800-nota-cad.

           IF ws-bim > 5 OR < 1
               DISPLAY (22 26) 'BIMESTRE INVALIDO'
               GO TO 0710-testa-bim.

           DISPLAY (22 20) 'X'.

        0800-nota-cad.
           ACCEPT (LIN , 61) ws-nota WITH PROMPT AUTO-SKIP
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              GO TO 0710-testa-bim.
           DISPLAY (22 20) linha-branco
           IF ws-nota < 0 OR > 10
              DISPLAY (22 35) 'NOTA INVALIDA'
              GO TO 0800-nota-cad.

       0900-nota-falta.
           IF ws-bim = 5
              MOVE ZEROS TO ws-faltas
              GO TO 1100-resp.
           ACCEPT (LIN , 71) ws-faltas WITH PROMPT AUTO-SKIP
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              GO TO 0800-nota-cad.
           DISPLAY (22 20) linha-branco
           IF ws-faltas < 0 OR > 20
              DISPLAY (22 28) 'NUMERO INVALIDO'
              GO TO 0900-nota-falta.

       1100-resp.
           DISPLAY (22 14) linha-branco
           DISPLAY (22 14)
              'CONFIRMA CADASTRO DA NOTA DO ALUNO? (S OU N)   [X]'
           ACCEPT (22 62) ws-resp-not WITH PROMPT AUTO-SKIP
           DISPLAY (22 14) linha-branco
           IF ws-resp-not = 'N' OR 'n'
              DISPLAY (22 27)'NOTA NAO CADASTRADA'
              GO TO 0710-testa-bim.
           IF ws-resp-not = 'S' OR 's'
              READ ENTNOT INVALID KEY
                   MOVE ws-bim TO nota-bim
                   MOVE ws-nota TO nota
                   MOVE ws-faltas TO falta
                   WRITE REG-ENTNOT
                   DISPLAY (22 29)'NOTA DO ALUNO CADASTRADA'
                   GO TO 1110-muda-linha.
              MOVE ws-bim TO nota-bim
              MOVE ws-nota TO nota
              MOVE ws-faltas TO falta
              REWRITE REG-ENTNOT
              DISPLAY (22 29)'NOTA DO ALUNO CADASTRADA'
              GO TO 1110-muda-linha.
           DISPLAY (22 30)'RESPOSTA INVALIDA'
           GO TO 1100-resp.

       1110-muda-linha.
           ADD 2 TO LIN
           IF LIN > 19
              GO TO 0700-notas.
           GO TO 0710-testa-bim.

       1200-finaliza.
           DISPLAY (22 17)
           "[ENTER] CONTINUAR  [F] FINALIZAR          [ ]".

       1300-resp.
           ACCEPT (22 60) ws-resp-not WITH PROMPT AUTO-SKIP
           DISPLAY (22 17) linha-branco
           IF ws-resp-not = SPACES
              GO TO 0200-TELA.

           IF ws-resp-not = "F" OR "f"
              CLOSE CADCUR
              CLOSE CADALU
              CLOSE CADDIS
              CLOSE ENTNOT
              CHAIN 'mnotas.exe'.

           DISPLAY(23 33) 'RESPOSTA INVALIDA'
           GO TO 1200-FINALIZA.

       1400-volta.
           DISPLAY (22 17)
           '[ENTER] CONTINUAR  [X] VOLTAR          [ ]'.

       1500-resp.
           ACCEPT (22 57) ws-voltar WITH PROMPT AUTO-SKIP
           DISPLAY (22 14) linha-branco
           IF ws-voltar = SPACES
              DISPLAY (23 33) linha-branco
              GO TO 0710-testa-bim.
           IF ws-voltar = "X" OR "x"
              GO TO 0200-tela.
           DISPLAY(23 33) 'RESPOSTA INVALIDA'
           GO TO 1400-volta.

