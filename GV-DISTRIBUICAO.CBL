000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. GV-DISTRIBUICAO.
000300*
000400* Sistema de Gestão de Vendas
      *
      *     Função: Distribuição da carteira de clientes para 
      *             os vendedores
      *     Autor: Mario Cesar Bernat
      *     Criação: Jun/2019
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECION.
       FILE-CONTROL.
           SELECT ARQ-CLIENTE ASSIGN TO DISK WID-ARQ-CLIENTE
             organization       is indexed
             access mode        is dynamic
             record key         is CLI-CHAVE-CLIENTE
             lock mode          is manual
             file status        is WS-RESULTADO-ACESSO.
           SELECT ARQ-VENDEDOR ASSIGN TO DISK WID-ARQ-VENDEDOR
             organization       is indexed
             access mode        is dynamic
             record key         is FOR-CHAVE-VENDEDOR
             lock mode          is manual
             file status        is WS-RESULTADO-ACESSO.
           SELECT ARQ-EXPORTACAO ASSIGN TO DISK
             organization       is line sequential.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       COPY GV-CADCLI.CPY.
       COPY GV-CADVEN.CPY.
       COPY GV-EXPCLI.CPY.
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-IDENTIFICACAO-ARQUIVOS.
           03 WID-ARQ-CLIENTE        PIC X(15) VALUE "GV-CLIENTE.DAT".
           03 WID-ARQ-VENDEDOR       PIC X(15) VALUE "GV-VENDEDOR.DAT".
           03 WID-ARQ-EXPORTACAO     PIC X(20) VALUE "DISTRIBUICAO.CSV".
      *
       01  WS-RESULTADO-ACESSO.
           03 WS-ESTADO-ARQUIVO      PIC X(02) VALUE "00".
              88 WS-OPERACAO-BEM-SUCEDIDA      VALUE "00".
              88 WS-ERRO-ACESSO                VALUE "39".
      *
       01  WS-CAMPOS-TRABALHO
           03 WS-OPCAO               PIC 9(01) VALUE ZEROS.
              88 WS-INCLUSAO                   VALUE 1.
              88 WS-ALTERACAO                  VALUE 2.
              88 WS-EXCLUSAO                   VALUE 3.
              88 WS-IMPORTACAO                 VALUE 4.
              88 WS-FIM-PROGRAMA               VALUE 0 9.
              88 WS-OPCAO-VALIDA               VALUE 1 2 3 4. 
           03 WS-MENSAGEM            PIC X(68).
           03 WS-RESPOSTA            PIC X(01).
              88 WS-SIM                        VALUE "S" "s".
              88 WS-NAO                        VALUE "N" "n".
           03 WS-ACAO                PIC X(01).
              88 WS-ACAO-INCLUSAO              VALUE "I".
              88 WS-ACAO-ALTERACAO             VALUE "A".
              88 WS-FIM-ACAO                   VALUE "F".
           03 WS-FINAL-ARQUIVO       PIC X(01) VALUE SPACE.
              88 WS-FIM-ARQUIVO                VALUE "F".
           03 WS-ERRO                PIC X(60).
           03 WS-QT-MENOR-DISTANCIA  PIX 9(15).
      *
       01  WS-LATITUDE-LONGITUDE.
           03 WS-DLAT                PIC 9(15).
           03 WS-LATITUDE-AUX        PIC S9(003)V9(008).
           03 WS-LATITUDE1           PIC S9(003)V9(008).
           03 WSR-LATITUDE1 REDEFINES WS-LATITUDE1.
              05 WS-LAT-GRA1         PIC S9(003).
              05 WS-LAT-MIN1         PIC 99
              05 WS-LAT-SEG1         PIC 99
              05 WS-LAT-MLS1         PIC 99
              05 WS-LAT-KLS1         PIC 99
           03 WS-LATITUDE2           PIC S9(003)V9(008).
           03 WSR-LATITUDE2 REDEFINES WS-LATITUDE2.
              05 WS-LAT-GRA2         PIC S9(003).
              05 WS-LAT-MIN2         PIC 99
              05 WS-LAT-SEG2         PIC 99
              05 WS-LAT-MLS2         PIC 99
              05 WS-LAT-KLS2         PIC 99  
           03 WS-DLAT                PIC 9(15).
           03 WS-LONGITUDE-AUX       PIC S9(003)V9(008).
           03 WS-LONGITUDE1          PIC S9(003)V9(008).
           03 WSR-LONGITUDE1 REDEFINES WS-LONGITUDE1.
              05 WS-LON-GRA1         PIC S9(003).
              05 WS-LON-MIN1         PIC 99
              05 WS-LON-SEG1         PIC 99
              05 WS-LON-MLS1         PIC 99
              05 WS-LON-KLS1         PIC 99
           03 WS-LONGITUDE2          PIC S9(003)V9(008).
           03 WSR-LONGITUDE2 REDEFINES WS-LONGITUDE2.
              05 WS-LON-GRA2         PIC S9(003).
              05 WS-LON-MIN2         PIC 99
              05 WS-LON-SEG2         PIC 99
              05 WS-LON-MLS2         PIC 99
              05 WS-LON-KLS2         PIC 99
      *
       SCREEN SECTION.
      *
       01  SST-LIMPA-TELA.
           03 LINE 22 COL 10 BLANK LINE.
           03 LINE 20 COL 01 BLANK LINE.
           03 LINE 19 COL 01 BLANK LINE.
           03 LINE 18 COL 01 BLANK LINE.
           03 LINE 17 COL 01 BLANK LINE.
           03 LINE 16 COL 01 BLANK LINE.
           03 LINE 15 COL 01 BLANK LINE.
           03 LINE 14 COL 01 BLANK LINE.
           03 LINE 13 COL 01 BLANK LINE.
           03 LINE 12 COL 01 BLANK LINE.
           03 LINE 11 COL 01 BLANK LINE.
           03 LINE 10 COL 01 BLANK LINE.
           03 LINE 09 COL 01 BLANK LINE.
           03 LINE 08 COL 01 BLANK LINE.
           03 LINE 07 COL 01 BLANK LINE.
           03 LINE 06 COL 01 BLANK LINE.
           03 LINE 05 COL 01 BLANK LINE.
           03 LINE 04 COL 01 BLANK LINE.
           03 LINE 02 COL 30 VALUE "                              ".
      *
       01  SST-DISTRIBUICAO.
           03 LINE 02 COL 30 VALUE "Distribuição da Carteira".           
      *
       01  SST-MENSAGEM.
           03 LINE 22 COL 11 USING WS-MENSAGE.
      *
       COPY GV-LINKAGE. 
      *
       PROCEDURE DIVISION USING LINKAGE-DADOS.
      *
       0-MODULO-CONTROLE SECTION.
      *-----------------
           PERFORM 1-MODULO-INICIAL.
           PERFORM 2-MODULO-PRINCIPAL.
           PERFORM 3-MODULO-FINAL.
       0-SAIDA.
           EXIT PROGRAM.
      *
       1-MODULO-INICIAL SECTION.
      *----------------
       1.
           DISPLAY SST-DISTRIBUICAO.
           OPEN I-O ARQ-CLIENTE.
           IF WS-ERRO-ACESSO 
              MOVE "F" TO WS-FINAL-ARQUIVO
              MOVE "Arquivo de Clientes em Uso." TO WS-MENSAGEM
              PERFORM 99-MENSAGEM.
           IF WS-OPERACAO-BEM-SUCEDIDA
              MOVE "F" TO WS-FIM-ARQUIVO
              MOVE "Arquivo de Clientes com Erro." TO WS-MENSAGEM
              PERFORM 99-MENSAGEM.
           OPEN INPUT ARQ-VENDEDOR.
           IF WS-ERRO-ACESSO 
              MOVE "F" TO WS-FINAL-ARQUIVO
              MOVE "Arquivo de Vendedores em Uso." TO WS-MENSAGEM
              PERFORM 99-MENSAGEM.
           IF WS-OPERACAO-BEM-SUCEDIDA
              MOVE "F" TO WS-FIM-ARQUIVO
              MOVE "Arquivo de Vendedores com Erro." TO WS-MENSAGEM
              PERFORM 99-MENSAGEM.             
       1-SAIDA.
           EXIT.
      *
       2-MODULO-PRINCIPAL SECTION.
      *------------------
       2.
           MOVE "Confirma Inclusão (S/N) ?" TO WS-MENSAGEM. 
           PERFORM 99-MENSAGEM.
           IF WS-SIM
              PERFORM 21-GERA-DISTRIBUICAO
              PERFORM 22-GERA-ARQUIVO.
       2-SAIDA.
           EXIT.
      *
       21-GERA-DISTRIBUICAO SECTION. 
      *--------------------
       21.
           MOVE ZEROS TO CLI-CD-CLIENTE
           START ARQ-CLIENTE KEY NOT LESS CLI-CHAVE-CLIENTE.
           PERFORM 99-LER-CLIENTE-NEXT.
           IF NOT WS-OPERACAO-BEM-SUCEDIDA
              MOVE "Erro na Leitura de Clientes." TO WS-MENSAGEM
              PERFORM 99-MENSAGEM
           ELSE
              PERFORM 211-TRATA-CLIENTE 
                   UNTIL WS-FIM-ACAO.  
       21-SAIDA.
           EXIT.
      *
       211-TRATA-CLIENTE SECTION
       211.
           MOVE 99999999999999 TO WS-QT-MENOR-DISTANCIA.
           MOVE ZEROS TO FOR-CD-VENDEDOR
                         WS-ESTADO-ARQUIVO.           
           START ARQ-VENDEDOR KEY NOT LESS FOR-CHAVE-VENDEDOR.
           PERFORM 2111-BUSCA-MENOR-DISTANCIA
                   UNTIL NOT WS-OPERACAO-BEM-SUCEDIDA.
       211-PROXIMO-CLIENTE.
           MOVE ZEROS TO WS-ESTADO-ARQUIVO.           
           PERFORM 99-LER-CLIENTE-NEXT
           IF NOT WS-OPERACAO-BEM-SUCEDIDA
              MOVE "F" TO WS-OPCAO.
       211-SAIDA.
           EXIT.
      *
       2111-BUSCA-MENOR-DISTANCIA
       2111.  
           PERFORM 99-CALCULA-DISTANCIA.
           IF WS-QT-CALCULO LESS WS-QT-MENOR-DISTANCIA 
              MOVE FOR-CHAVE-VENDEDOR CLI-CD-VENDEDOR
              MOVE WS-QT-CALCULO TO WS-QT-MENOR-DISTANCIA
                                    CLI-QT-DISTANCIA.
       2111-PROXIMO-VENDEDOR.
           PERFORM 99-LER-VENDEDOR-NEXT.
           IF NOT WS-OPERACAO-BEM-SUCEDIDA
              PERFORM 99-REGRAVAR-CLIENTE.
       2111-SAIDA.
           EXIT.
      *
       22-GERA-ARQUIVO SECTION.
      *---------------
       22.
           MOVE SPACES TO WS-ERRO.
           OPEN OUTPUT ARQ-EXPORTACAO.
           IF WS-OPERACAO-BEM-SUCEDIDA
              MOVE "Erro na Criação do Arquivo DISTRIB.CSV." TO WS-ERRO. 
      *
           IF WS-ERRO EQUAL SPACES
              MOVE ZEROS TO CLI-CD-CLIENTE
                            WS-ESTADO-ARQUIVO           
              START ARQ-CLIENTE KEY NOT LESS CLI-CHAVE-CLIENTE
              PERFORM 99-LER-CLIENTE-NEXT
              IF NOT WS-OPERACAO-BEM-SUCEDIDA
                 MOVE "Erro na Leitura de Clientes." TO WS-ERRO. 

           IF WS-ERRO NOT EQUAL SPACES
              PERFORM 99-MENSAGEM
           ELSE
              PERFORM 221-MONTA-ARQUIVO.
                        UNTIL WS-FIM-ACAO 
              PERFORM 99-FECHAR-ARQUIVO.
      *
       22-SAIDA.
           EXIT.
      *
       221-MONTA-ARQUIVO SECTION.
       221.
           MOVE CLI-CD-CLIENTE      TO ARQ-CD-CLIENTE.
           MOVE CLI-DS-RAZAO-SOCIAL TO ARQ-DS-RAZAO-SOCIAL.
           MOVE CLI-CD-VENDEDOR     TO ARQ-CD-VENDEDOR
                                       FOR-CD-VENDEDOR.
           MOVE CLI-QT-DISTANCIA    TO ARQ-QT-DISTRIBUICAO.
           MOVE ";" TO ARQ-DELIMITADOR-1 ARQ-DELIMITADOR-2 
                       ARQ-DELIMITADOR-3 ARQ-DELIMITADOR-4.
      *
           PERFORM 99-LER-VENDEDOR.
           IF NOT WS-OPERACAO-BEM-SUCEDIDA
              MOVE SPACES           TO ARQ-NM-VENDEDOR
           ELSE
              MOVE FOR-NM-VENDEDOR  TO ARQ-NM-VENDEDOR.
           PERFORM 99-GRAVA-ARQUIVO.
      *
       221-LER-PROXIMO.
           PERFORM 99-LER-CLIENTE-NEXT.
           IF NOT WS-OPERACAO-BEM-SUCEDIDA
              MOVE "F" TO WS-OPCAO.
       221-SAIDA.
           EXIT.
      *
       3-MODULO-FINAL SECTION.
      *--------------
       3.
           CLOSE ARQ-CLIENTE.
           CLOSE ARQ-VENDEDOR.
           DISPLAY SST-LIMPA-TELA.
       3-SAIDA.
           EXIT.
     *
       99-ROTINAS-DIVERSAS SECTION.
      *-------------------
      *
       99-LER-CLIENTE.
           READ ARQ-CLIENTE.
      *
       99-LER-CLIENTE-NEXT.
           READ ARQ-CLIENTE NEXT.
      * 
       99-REGRAVA-CLIENTE.
           REWRITE CLI-REGISTRO-CLIENTE.
      *
       99-LER-VENDEDOR-NEXT.
           READ ARQ-VENDEDOR NEXT.
      *
       99-LER-IMPORTACAO.
           READ ARQ-REGISTRO-CLIENTE 
               AT END MOVE "F" TO WS-FIM-ARQUIVO.
      *
       99-FECHA-ARQUIVO-SAIDA.
           CLOSE ARQ-EXPORTACAO.
      *
       99-GRAVA-ARQUIVO.
           WRITE ARQ-REGISTRO-DISTRIBUICAO.
      *
       99-CALCULA-DISTANCIA
           MOVE CLI-NR-LATITUDE  TO WS-LATITUDE1
           MOVE CLI-NR-LONGITUDE TO WS-LONGITUDE1
           MOVE FOR-NR-LATITUDE  TO WS-LATITUDE2
           MOVE FOR-NR-LONGITUDE TO WS-LONGITUDE2
           IF WS-LATITUDE1 < 0
              COMPUTE WS-LATITUDE1 = WS-LATITUDE1 * (-1).
           IF WS-LATITUDE2 < 0
              COMPUTE WS-LATITUDE2 = WS-LATITUDE2 * (-1).
           IF WS-LONGITUDE1 < 0
              COMPUTE WS-LONGITUDE1 = WS-LONGITUDE1 * (-1).
           IF WS-LONGITUDE2 < 0
              COMPUTE WS-LONGITUDE2 = WS-LONGITUDE2 * (-1).
      *
           IF WS-LATITUDE1 < WS-LATITUDE2
              MOVE WS-LATITUDE1 TO WS-LATITUDE-AUX
              MOVE WS-LATITUDE2 TO WS-LATITUDE1
              MOVE WS-LATITUDE-AUX TO WS-LATITUDE2.
           IF WS-LONGITUDE1 < WS-LONGITUDE2
              MOVE WS-LONGITUDE1 TO WS-LONGITUDE-AUX
              MOVE WS-LONGITUDE2 TO WS-LONGITUDE1
              MOVE WS-LONGITUDE-AUX TO WS-LONGITUDE2.
      *
           COMPUTE WS-DLAT = ( ((WS-LAT-GRA1 - WS-LAT-GRA2) * 60)
                           +    (WS-LAT-MIN1 - WS-LAT-MIN2)
                           +   ((WS-LAT-SEG1 - WS-LAT-SEG2) / 60)   
                           +   ((WS-LAT-MLS1 - WS-LAT-MLS2) / 6000)   
                           +   ((WS-LAT-KLS1 - WS-LAT-KLS2) / 600000)
                             ) * 1852.   
           COMPUTE WS-DLON = ( ((WS-LON-GRA1 - WS-LON-GRA2) * 60)
                           +    (WS-LON-MIN1 - WS-LON-MIN2)
                           +   ((WS-LON-SEG1 - WS-LON-SEG2) / 60)   
                           +   ((WS-LON-MLS1 - WS-LON-MLS2) / 6000)   
                           +   ((WS-LON-KLS1 - WS-LON-KLS2) / 600000)
                             ) * 1852.   
           COMPUTE WS-QT-CALCULO = ( (WS-DLAT * WS-DLAT) 
                                   + (WS-DLON * WS-DLON) ) ** (1 / 2).
      *
       99-MENSAGEM.
           DISPLAY SST-MENSAGEM.
           ACCEPT  SST-MENSAGEM.
      *
