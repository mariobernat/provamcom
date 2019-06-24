000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. GV-CADVENDEDOR.
000300*
000400* Sistema de Gestão de Vendas
      *
      *     Função: Cadastro de Vendedores
      *     Autor: Mario Cesar Bernat
      *     Criação: Jun/2019
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECION.
       FILE-CONTROL.
           SELECT ARQ-VENDEDOR ASSIGN TO DISK WID-ARQ-VENDEDOR
             organization       is indexed
             access mode        is dynamic
             record key         is FOR-CHAVE-VENDEDOR
             lock mode          is manual
             file status        is WS-RESULTADO-ACESSO.
           SELECT ARQ-IMPORTACAO ASSIGN TO DISK
             organization       is line sequential.
      *
       DATA DIVISION.
       FILE SECTION.
       COPY GV-CADVEN.CPY.
       COPY GV-IMPVEN.CPY.
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-IDENTIFICACAO-ARQUIVOS.
           03 WID-ARQ-VENDEDOR       PIC X(15) VALUE "GV-VENDEDOR.DAT".
           03 WID-ARQ-IMPORTACAO     PIC X(20) VALUE SPACES.
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
      *
      * DADOS DO VENDEDOR
           03 WS-CD-VENDEDOR             PIC 9(007).
           03 WS-NR-CPF-VENDEDOR         PIC 9(011).
           03 WS-NM-VENDEDOR             PIC X(040).
           03 WS-NR-LATITUDE             PIC ZZ9,99999999.
           03 WS-NR-LONGITUDE            PIC ZZ9,99999999.
      *
      * CALCULO DO DIGITO CNPJ/CPF
        COPY GV-VALIDACPF1.CPY.
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
       01  SST-LIMPA-REGISTRO.
           03 LINE 07 COL 27 VALUE "                    ".
           03 LINE 08 COL 27 VALUE 
                        "                                        ".
           03 LINE 09 COL 27 VALUE "                    ".
           03 LINE 10 COL 27 VALUE "                    ".
           03 LINE 11 COL 27 VALUE "                    ".
      *
       01  SST-CADASTRO-VENDEDOR.
           03 LINE 02 COLUMN 30 "Cadastro de Vendedores".           
           03 LINE 05 COLUMN 08 "Informe a Opção: ".
           03 LINE 05 COLUMN 28 
              "(1-Inclusão, 2-Alteração, 3-Exclusão, 4-Importação)".
      *
       01  SST-CADASTRO-VENDEDOR.
           03 LINE 02 COL 30 VALUE "Cadastro de Vendedores".           
           03 LINE 05 COL 08 VALUE "Informe a Opção: ".
           03 OPCAO LINE 05 COL 26 PIC 9 USING WS-OPCAO AUTO
           03 LINE 05 COL 28 VALUE 
              "(1-Inclusão, 2-Alteração, 3-Exclusão, 4-Importação)".
      *
       01  SST-CD-VENDEDOR.
           03 LINE 07 COL 06 VALUE " Códido do Vendedor: ".
           03 LINE 07 COL 26 USING WS-CD-VENDEDOR.
      *
       01  SST-DADOS-VENDEDOR.
           03 LINE 08 COL 07 VALUE "               CPF: ".
           03 LINE 08 COL 26 USING WS-NR-CPF-VENDEDOR.
           03 LINE 09 COL 07 VALUE "              Nome: ".
           03 LINE 09 COL 26 USING WS-NM-NOME.
           03 LINE 10 COL 07 VALUE "          Latitude: ".
           03 LINE 10 COL 26 USING WS-NR-LATITUDE.
           03 LINE 11 COL 07 VALUE "         Longitude: ".
           03 LINE 11 COL 26 USING WS-NR-LONGITUDE.
      *
       01  SST-IMPORTA-VENDEDOR.
           03 LINE 14 COL 07 VALUE "Informe o Arquivo a ser Importado:".
           03 LINE 14 COL 42 USING WID-ARQUIVO-ENTRADA. 
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
       0.
           PERFORM 1-MODULO-INICIAL.
           PERFORM 2-MODULO-PRINCIPAL
               UNTIL WS-FIM-PROGRAMA.
           PERFORM 3-MODULO-FINAL.
       0-SAIDA.
           EXIT PROGRAM.
      *
       1-MODULO-INICIAL SECTION.
      *----------------
       1.
           OPEN I-O ARQ-VENDEDOR.
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
           ACCEPT SST-CADASTRO-VENDEDOR.
           IF  WS-INCLUSAO
               PERFORM 21-INCLUI-VENDEDOR
                          UNTIL WS-FIM-ACAO. 
           IF  WS-ALTERACAO
               PERFORM 22-ALTERA-VENDEDOR
                          UNTIL WS-FIM-ACAO. 
           IF  WS-EXCLUSAO
               PERFORM 23-EXCLUI-VENDEDOR
                          UNTIL WS-FIM-ACAO. 
           IF  WS-IMPORTACAO
               PERFORM 24-IMPORTA-VENDEDOR
                          UNTIL WS-FIM-ACAO. 
       2-SAIDA.
           EXIT.
      *
       21-INCLUI-VENDEDOR SECTION. 
      *------------------
       21.
           ACCEPT SST-CD-VENDEDOR
           IF WS-CD-VENDEDOR EQUAL ZEROS
              MOVE "F" TO TO WS-OPCAO
           ELSE
              PERFORM 211-TRATAR-VENDEDOR.          
       21-SAIDA.
           EXIT.
      *
       211-TRATAR-VENDEDOR SECTION.
       211.
           ACCEPT SST-DADOS-VENDEDOR.
           MOVE "Confirma Inclusão (S/N) ?" TO WS-MENSAGEM. 
           PERFORM 99-MENSAGEM.
           IF WS-SIM
      *
      *       Valida o codigo do VENDEDOR ----
              MOVE WS-CD-VENDEDOR TO FOR-NR-CPF-VENDEDOR
              PERFORM 99-LER-VENDEDOR
              IF WS-OPERACAO-BEM-SUCEDIDA
                 MOVE "Codigo do Vendedor já cadastrado!" TO WS-ERRO
              ELSE
      *
      *          Valida se o CPF já foi existe ----            
                 MOVE WS-NR-CPF-VENDEDOR TO FOR-NR-CPF-VENDEDOR
                                              WS-CPF
                 MOVE ZEROS TO FOR-CD-VENDEDOR
                               WS-ESTADO-ARQUIVO
                 START ARQ-VENDEDOR KEY NOT LESS FOR-CHAVE-VENDEDOR
                 PERFORM 99-LER-VENDEDOR-NEXT
                       UNTIL NOT WS-OPERACAO-BEM-SUCEDIDA
                       OR  (WS-NR-CFP-VENDEDOR = FOR-NR-CPF-VENDEDOR
                       AND (WS-CD-VENDEDOR NOT EQUAL FOR-CD-VENDEDOR)
                 IF  WS-OPERACAO-BEM-SUCEDIDA
                 AND WS-NR-CPF-VENDEDOR EQUAL FOR-NR-CPF-VENDEDOR
                    MOVE "CPF já Cadastrado" TO WS-ERRO
                 ELSE
      *
      *             Valida o CPF do VENDEDOR ----            
                    PERFORM 99-VALIDA-CPF
                    IF WS-NR-CPF-VENDEDOR NOT EQUAL WS-CPF 
                       MOVE "CPF Invalido!" TO WS-ERRO.
      *
       211-MOVE-DADOS.
           IF WS-SIM
              IF WS-ERRO NOT EQUAL SPACES
                 MOVE WS-ERRO TO WS-MENSAGEM
                 PERFORM 99-MENSAGEM
              ELSE
                 MOVE WS-CD-VENDEDOR     TO FOR-CD-VENDEDOR
                 MOVE WS-NR-CPF-VENDEDOR TO FOR-NR-CPF-VENDEDOR
                 MOVE WS-NM-VENDEDOR     TO FOR-NM-VENDEDOR
                 MOVE WS-NR-LATITUDE     TO FOR-NR-LATITUDE
                 MOVE WS-NR-LONGITUDE    TO FOR-NR-LONGITUDE
                 PERFORM 99-GRAVA-VENDEDOR.
       211-SAIDA.
           EXIT.
      *
       22-ALTERA-VENDEDOR SECTION.
      *------------------
       22.
           ACCEPT SST-CD-VENDEDOR.
           MOVE WS-CD-VENDEDOR   TO FOR-CD-VENDEDOR.
           IF WS-CD-VENDEDOR EQUAL ZEROS
              MOVE "F" TO TO WS-OPCAO
           ELSE 
              PERFORM 99-LER-VENDEDOR
              IF NOT WS-OPERACAO-BEM-SUCEDIDA
                 MOVE "Código do Vendedor não Encontrado." TO
                      WS-MENSAGEM
                 PERFORM 99-MENSAGEM
              ELSE
                 MOVE FOR-NR-CPF-VENDEDOR TO WS-NR-CPF-VENDEDOR
                 MOVE FOR-NM-VENDEDOR     TO WS-NM-VENDEDOR
                 MOVE FOR-NR-LATITUDE     TO WS-NR-LATITUDE
                 MOVE FOR-NR-LONGITUDE    TO WS-NR-LONGITUDE
                 PERFORM 221-TRATA-VENDEDOR.
       221-SAIDA.
           EXIT.
      *
       221-TRATA-VENDEDOR SECTION.
       221.
           ACCEPT SST-DADOS-VENDEDOR.
           MOVE "Confirma Alteração (S/N) ?" TO WS-MENSAGEM. 
           PERFORM 99-MENSAGEM.
           IF WS-SIM
      *
      *       Valida se o CPF já foi existe ----            
              MOVE WS-NR-CPF-VENDEDOR TO FOR-NR-CPF-VENDEDOR
                                         WS-CPF
              MOVE ZEROS TO FOR-CD-VENDEDOR
                            WS-ESTADO-ARQUIVO
              START ARQ-VENDEDOR KEY NOT LESS FOR-CHAVE-VENDEDOR
              PERFORM 99-LER-VENDEDOR-NEXT
                      UNTIL NOT WS-OPERACAO-BEM-SUCEDIDA
                       OR  (WS-NR-CPF-VENDEDOR EQUAL FOR-NR-CPF-VENDEDOR
                       AND (WS-CD-VENDEDOR NOT EQUAL FOR-CD-VENDEDOR)
              IF  WS-OPERACAO-BEM-SUCEDIDA
              AND WS-NR-CPF-VENDEDOR EQUAL FOR-NR-CPF-VENDEDOR
                 MOVE "CPF já Cadastrado" TO WS-ERRO
              ELSE
      *
      *          Valida o CPF do VENDEDOR ----            
                 PERFORM 99-VALIDA-CPF
                 IF WS-NR-CPF-VENDEDOR NOT EQUAL WS-CPF 
                    MOVE "CPF Invalido!" TO WS-ERRO.
      *
       221-MOVE-DADOS.
           IF WS-SIM
              IF WS-ERRO NOT EQUAL SPACES
                 MOVE WS-ERRO TO WS-MENSAGEM
                 PERFORM 99-MENSAGEM
              ELSE
                 MOVE WS-CD-VENDEDOR     TO FOR-CD-VENDEDOR
                 MOVE WS-NR-CPF-VENDEDOR TO FOR-NR-CPF-VENDEDOR
                 MOVE WS-NM-VENDEDOR     TO FOR-NM-VENDEDOR
                 MOVE WS-NR-LATITUDE     TO FOR-NR-LATITUDE
                 MOVE WS-NR-LONGITUDE    TO FOR-NR-LONGITUDE
                 PERFORM 99-REGRAVA-VENDEDOR
                 IF NOT WS-OPERACAO-BEM-SUCEDIDA
                    MOVE "Erro ao Excluir o Vendedor." TO WS-MENSAGEM
                    PERFORM 99-MENSAGEM
                 ELSE
                    DISPLAY SST-LIMPA-REGISTRO. 
       221-SAIDA.
           EXIT.
      *
       23-EXCLUI-VENDEDOR SECTION.
      *-----------------
       23.
           ACCEPT SST-CD-VENDEDOR.
           MOVE WS-CD-VENDEDOR  TO FOR-CD-VENDEDOR
           IF WS-CD-VENDEDOR EQUAL ZEROS
              MOVE "F" TO TO WS-OPCAO.
      *
       23-TRATA-VENDEDOR.
           IF WS-CD-VENDEDOR > ZEROS
              PERFORM 99-LER-VENDEDOR
              IF NOT WS-OPERACAO-BEM-SUCEDIDA
                 MOVE "Código do Vendedor não Encontrado." TO 
                      WS-MENSAGEM
                 PERFORM 99-MENSAGEM
              ELSE
                 MOVE FOR-NR-CPF-VENDEDOR TO WS-NR-CPF-VENDEDOR
                 MOVE FOR-NM-VENDEDOR     TO WS-NM-VENDEDOR
                 MOVE FOR-NR-LATITUDE     TO WS-NR-LATITUDE
                 MOVE FOR-NR-LONGITUDE    TO WS-NR-LONGITUDE
                 DISPLAY SST-DADOS-VENDEDOR
                 MOVE "Confirma os Exclusão (S/N) ?" TO WS-MENSAGEM
                 PERFORM 99-MENSAGEM
                 IF WS-SIM
                    PERFORM 99-DELETA-VENDEDOR
                    IF NOT WS-OPERACAO-BEM-SUCEDIDA
                       MOVE "Erro ao Excluir o Vendedor." TO WS-MENSAGEM
                       PERFORM 99-MENSAGEM.
                    ELSE 
                       DISPLAY SST-LIMPA-REGISTRO. 
       23-SAIDA.
           EXIT.
      *
       24-IMPORTA-VENDEDOR SECTION.
      *------------------
       24.
           DISPLAY SST-IMPORTA-CLIENTE.
           IF WID-ARQUIVO-ENTRADA EQUAL SPACE
              MOVE "F" TO WS-OPCAO
           ELSE
              MOVE "Confirma os dados (S/N) ?" TO WS-MENSAGEM
              PERFORM 99-MENSAGEM
              IF WS-SIM
                 PERFORM 241-TRATA-IMPORTACAO.
       24-SAIDA.
           EXIT.
      *
       241-TRATA-IMPORTACAO SECTION.
       241.
           OPEN INPUT ARQUIVO-ENTRADA.
           IF WS-OPERACAO-BEM-SUCEDIDA
              MOVE SPACE TO WS-FIM-ARQUIVO
              PERFORM 99-LER-ARQUIVO-ENTRADA
              PERFORM 2411-TRATA-REGISTROS
                      UNTIL WS-FIM-ARQUIVO
              CLOSE ARQ-IMPORTACAO
           ELSE
              MOVE "Arquivo não encontrado ou com erro!" 
                   TO WS-MENSAGEM 
              PERFORM 99-MENSAGEM.
       241-SAIDA.
           EXIT.
      *
       2411-TRATA-REGISTROS SECTION.
       2411. 
           MOVE SPACES TO WS-ERRO 
                          WS-ACAO.
           MOVE ARQ-CD-VENDEDOR      TO FOR-CD-VENDEDOR.
           START ARQ-VENDEDOR KEY EQUAL FOR-CHAVE-VENDEDOR.
           PERFORM 99-LER-VENDEDOR-NEXT.
           IF WS-OPERACAO-BEM-SUCEDIDA
              IF ARQ-CD-VENDEDOR EQUAL FOR-CD-VENDEDOR
                 MOVE "A" TO WS-ACAO
              ELSE
                 MOVE "I" TO WS-ACAO
           ELSE
              MOVE "I" TO WS-ACAO.
      *
           MOVE ARQ-NR-CPF-VENDEDOR TO FOR-NR-CPF-VENDEDOR.
           MOVE ZEROS TO FOR-CD-VENDEDOR
                         WS-ESTADO-ARQUIVO.           
           START ARQ-VENDEDOR KEY NOT LESS FOR-CHAVE-VENDEDOR.
           PERFORM 99-LER-VENDEDOR-NEXT
                   UNTIL NOT WS-OPERACAO-BEM-SUCEDIDA
                      OR (ARQ-NR-CPF-VENDEDOR EQUAL FOR-NR-CPF-VENDEDOR
                     AND (ARQ-CD-VENDEDOR NOT EQUAL FOR-CD-VENDEDOR).
      *
           IF ARQ-NR-CPF-VENDEDOR EQUAL FOR-NR-CPF-VENDEDOR
           AND WS-ACAO = "I"
              MOVE "CPF já Cadastrado" TO WS-ERRO.
      *
           MOVE ARQ-NR-CPF-VENDEDOR TO FOR-NR-CPF-VENDEDOR
                                       WS-CPF.
           PERFORM 99-VALIDA-CPF.
           IF ARQ-NR-CPF-VENDEDOR NOT EQUAL WS-CPF 
              MOVE "CPF Invalido" TO WS-ERRO.
      *
       241-MOVE-DADOS.
           IF WS-ERRO EQUAL SPACES
              MOVE ARQ-CD-VENDEDOR     TO FOR-CD-VENDEDOR
              MOVE ARQ-NR-CPF-VENDEDOR TO FOR-NR-CPF-VENDEDOR
              MOVE ARQ-NM-VENDEDOR     TO FOR-NM-VENDEDOR
              MOVE ARQ-NR-LATITUDE     TO FOR-NR-LATITUDE
              MOVE ARQ-NR-LONGITUDE    TO FOR-NR-LONGITUDE
              IF WS-ACAO-INCLUSAO
                 PERFORM 99-GRAVA-VENDEDOR
              ELSE 
                 PERFORM 99-REGRAVA-VENDEDOR.
      *
       241-LER-PROXIMO.
           PERFORM 99-LER-ARQUIVO-ENTRADA.
      * 
       241-SAIDA.
           EXIT.
      *
       3-MODULO-FINAL SECTION.
      *--------------
       3.
           CLOSE ARQ-VENDEDOR.
           DISPLAY SST-LIMPA-TELA.
       3-SAIDA.
           EXIT.
      *
       99-ROTINAS-DIVERSAS SECTION.
      *-------------------
      *
       99-LER-VENDEDOR.
           READ ARQ-VENDEDOR.
      *
       99-LER-VENDEDOR-NEXT.
           READ ARQ-VENDEDOR NEXT.
      * 
       99-GRAVA-VENDEDOR.
           WRITE FOR-REGISTRO-VENDEDOR.
      *
       99-REGRAVA-VENDEDOR.
           REWRITE FOR-REGISTRO-VENDEDOR.
      *
       99-LER-IMPORTACAO.
           READ ARQ-REGISTRO-VENDEDOR 
               AT END MOVE "F" TO WS-FIM-ARQUIVO.
      *
       99-FECHA-ARQUIVO-ENTRADA.
           CLOSE ARQ-IMPORTACAO.
      *
       COPY GV-VALIDACPF2.CPY.
      *
       99-MENSAGEM.
           DISPLAY SST-MENSAGEM.
           ACCEPT  SST-MENSAGEM.
      *
