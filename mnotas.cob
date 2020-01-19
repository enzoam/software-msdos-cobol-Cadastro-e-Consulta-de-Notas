       IDENTIFICATION DIVISION.
       PROGRAM-ID MNOTAS-COB.
      *    FESP - FUNDACAO DE ESTUDOS SOCIAIS DO PARANA
      *    ANALISTA         :ENZO 19 - JAMILE 26
      *    PROGRAMADOR(A)   :ENZO 19 - JAMILE 26
      *    DATA             :
      *    FINALIDADE       :SUBMENU NOTAS E FALTAS
      *                      - ENTRADA DE NOTAS E FALTAS
      *                      - CADASTRO E CONSULTA
      *    VRS              DATA              DESCRICAO
      *    1.5              25/10/2000        TRABALHO.

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
          05 line  05   column 15  value 'C A D A S T R O  D E  N O T A
      -                                  'S  E  F A L T A S'.
          05 line  08   column 10  value 'E N T R A D A  D E  N O T A S               
      -                                  '-------------------------[ ]'.
          05 line  10   column 10  value 'C A D A S T R O  E  C O N S U               
      -                                  'L T A -------------------[ ]'.
          05 line  12   column 10  value 'R E L A T O R I O ------------
      -                                  '-------------------------[ ]'.
          05 line  14   column 10  value 'V O L T A R ------------------
      -                                  '-------------------------[ ]'.

          05 line  18   column 25  value 'MARQUE COM UM <X> A OPCAO'.
      

       PROCEDURE DIVISION.

       0100-inicio.
            DISPLAY tela-menu.

       0200-inclusao.
            ACCEPT (08 66) ws-opcao with prompt AUTO-SKIP
            DISPLAY (23 03) ws-limpa-tela
            IF ws-opcao = 'X' or 'x'
               CHAIN 'entnotas.EXE'
               GO TO 0100-inicio.
            IF ws-opcao = spaces
               GO TO 0300-alteracao.
            DISPLAY (23 29) 'RESPOSTA INVALIDA'.
               GO TO 0200-inclusao.

       0300-alteracao.
            ACCEPT (10 66) ws-opcao with prompt AUTO-SKIP
            DISPLAY (23 03) ws-limpa-tela
            IF ws-opcao = 'X' or 'x'
               CHAIN 'cadnotas.EXE'
               GO TO 0100-inicio.
           IF ws-opcao = spaces
               GO TO 0400-relatorio.
           DISPLAY (23 29) 'RESPOSTA INVALIDA'.
               GO TO 0300-alteracao.

       0400-relatorio.
            ACCEPT (12 66) ws-opcao with prompt AUTO-SKIP
            DISPLAY (23 03) ws-limpa-tela
            IF ws-opcao = 'X' or 'x'
               CHAIN 'relnot.EXE'
               GO TO 0100-inicio.
           IF ws-opcao = spaces
               GO TO 0600-sair.
           DISPLAY (23 29) 'RESPOSTA INVALIDA'.
               GO TO 0400-relatorio.

       0600-sair.
            ACCEPT (14 66) ws-opcao with prompt AUTO-SKIP
            DISPLAY (23 03) ws-limpa-tela
            IF ws-opcao = 'X' or 'x'
               CHAIN 'FACAD.EXE'
               GO TO 0100-inicio.
            IF ws-opcao = spaces
               GO TO 0200-inclusao.
           DISPLAY (23 29) 'RESPOSTA INVALIDA'.
               GO TO 0600-sair.


