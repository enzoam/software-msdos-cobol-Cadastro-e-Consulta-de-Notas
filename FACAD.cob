       IDENTIFICATION DIVISION.
       PROGRAM-ID FACAD-COB.
      *    FESP - FUNDACAO DE ESTUDOS SOCIAIS DO PARANA
      *    ANALISTA         :ENZO 19 - JAMILE 26
      *    PROGRAMADOR(A)   :ENZO 19 - JAMILE 26
      *    DATA             :
      *    FINALIDADE       :PROGRAMA DE CONTROLE DE FACULDADE
      *                      - ALUNOS
      *                      - CURSOS
      *                      - DISCIPLINAS
      *                      - RELATORIO
      *                      - NOTAS E FALTAS
      *    VRS              DATA              DESCRICAO
      *    1.5              10/11/2000        TRABALHO.

       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 ws-opcao                  PIC X.
       01 ws-resp                   PIC X.
       01 ws-limpa-tela             PIC X(70) VALUE SPACES.

       SCREEN SECTION.
       01 tela-menu.
          05 blank screen.
          05 line  02   column 02  value 'SISTEMA - FACAD'.
          05 line  02   column 29  value 'CONTROLE DE FACULDADE'.
          05 line  02   column 72  value 'VRS 0.0'.
          05 line  05   column 26  value 'M E N U * P R I N C I P A L'.
          05 line  08   column 10  value 'A L U N O S ------------------               
      -                                  '-------------------------[ ]'.
          05 line  10   column 10  value 'C U R S O S ------------------               
      -                                  '-------------------------[ ]'.
          05 line  12   column 10  value 'D I S C I P L I N A S --------  
      -                                  '-------------------------[ ]'.
          05 line  14   column 10  value 'N O T A S  E  F A L T A S ----  
      -                                  '-------------------------[ ]'.
          05 line  16   column 10  value 'R E L A T O R I O ------------
      -                                  '-------------------------[ ]'.
          05 line  18   column 10  value 'S A I R ----------------------
      -                                  '-------------------------[ ]'.
          05 line  20   column 25  value 'MARQUE COM UM <X> A OPCAO'.
      

       PROCEDURE DIVISION.

       0100-inicio.
            DISPLAY tela-menu.

       0200-alunos.
            ACCEPT (08 66) ws-opcao with prompt AUTO-SKIP
            DISPLAY (23 03) ws-limpa-tela
            IF ws-opcao = 'X' or 'x'
               CHAIN 'MALUNOS.EXE'.
            IF ws-opcao = spaces
               GO TO 0300-cursos.
            DISPLAY (23 29) 'RESPOSTA INVALIDA'.
               GO TO 0200-alunos.

       0300-cursos.
            ACCEPT (10 66) ws-opcao with prompt AUTO-SKIP
            DISPLAY (23 03) ws-limpa-tela
            IF ws-opcao = 'X' or 'x'
               CHAIN 'MCURSOS.EXE'.
            IF ws-opcao = spaces
               GO TO 0350-disciplinas.
            DISPLAY (23 29) 'RESPOSTA INVALIDA'.
               GO TO 0300-cursos.

       0350-disciplinas.
            ACCEPT (12 66) ws-opcao with prompt AUTO-SKIP
            DISPLAY (23 03) ws-limpa-tela
            IF ws-opcao = 'X' or 'x'
               CHAIN 'MDISCIPL.EXE'.
            IF ws-opcao = spaces
               GO TO 0400-notas-faltas.
            DISPLAY (23 29) 'RESPOSTA INVALIDA'.
               GO TO 0350-disciplinas.

            
       0400-notas-faltas.
            ACCEPT (14 66) ws-opcao with prompt AUTO-SKIP
            DISPLAY (23 03) ws-limpa-tela
            IF ws-opcao = 'X' or 'x'
               CHAIN 'MNOTAS.EXE'.
            IF ws-opcao = spaces
               GO TO 0500-relatorio.
           DISPLAY (23 29) 'RESPOSTA INVALIDA'.
               GO TO 0400-notas-faltas.

       0500-relatorio.
            ACCEPT (16 66) ws-opcao with prompt AUTO-SKIP
            DISPLAY (23 03) ws-limpa-tela
            IF ws-opcao = 'X' or 'x'
               CHAIN 'MENUREL.EXE'.
            IF ws-opcao = spaces
               GO TO 0600-sair.
           DISPLAY (23 29) 'RESPOSTA INVALIDA'.
               GO TO 0500-relatorio.

       0600-sair.
            ACCEPT (18 66) ws-opcao with prompt AUTO-SKIP
            DISPLAY (23 03) ws-limpa-tela
            IF ws-opcao = 'X' or 'x'
               GO TO 0700-confirma.
            IF ws-opcao = spaces
               GO TO 0100-inicio.
            DISPLAY (23 29) 'RESPOSTA INVALIDA'.
               GO TO 0600-sair.

       0700-confirma.
            DISPLAY (20 20) 'ENTER - Continua     F - Finaliza  [ ? ]'.

       0800-resp.
           ACCEPT (20 57) ws-resp with prompt AUTO-SKIP
           DISPLAY (23 03) ws-limpa-tela
           IF ws-resp = SPACES
              GO TO 0100-inicio.
           IF ws-resp = 'F' or 'f'
              STOP RUN.
           DISPLAY (23 29) 'RESPOSTA INVALIDA'
           GO TO 0800-resp.
