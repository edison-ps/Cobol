      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGAB03       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Relacao do cadastro :                                      *
      *                                                             *
      *  Data da ultima alteracao:    23/11/93     v1.00            *
      *                               01/03/94     v1.01            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgab03.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqab01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab01-chave
                  alternate record key is ab01-chave-1 with duplicates
                  alternate record key is ab01-chave-2 with duplicates
                  file status is ab01-status.
      *
           select arqab02 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab02-chave
                  alternate record key is ab02-chave-1 with duplicates
                  file status is ab02-status.
      *
           select arqab04 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab04-chave
                  alternate record key is ab04-chave-1 with duplicates
                  alternate record key is ab04-chave-2 with duplicates
                  file status is ab04-status.
      *
           select arqimp assign to disk
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
       data division.
       file section.
      *    
       copy fdab01.lib.
      *    
       copy fdab02.lib.
      *    
       copy fdab04.lib.
      *
       fd arqimp
      
       label record is standard
       value of file-id is impress
       data record is reg-imp.

       01 reg-imp                      pic x(300).
      *
       working-storage section.
      *
       01 ab01-status                  pic x(02) value "00".
       01 ab01-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab01.
          02 ab01-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab01-nome                 pic x(08) value "ARQAB01A".
          02 filler                    pic x(01) value ".".
          02 ab01-ext                  pic x(03) value "DAT".
      *
       01 ab02-status                  pic x(02) value "00".
       01 ab02-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab02.
          02 ab02-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab02-nome                 pic x(08) value "ARQAB02A".
          02 filler                    pic x(01) value ".".
          02 ab02-ext                  pic x(03) value "DAT".
      *
       01 ab04-status                  pic x(02) value "00".
       01 ab04-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab04.
          02 ab04-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab04-nome-arq             pic x(08) value "ARQAB04A".
          02 filler                    pic x(01) value ".".
          02 ab04-ext                  pic x(03) value "DAT".
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGAB03".
          02 cb-versao                 pic x(06) value "v1.01 ".
      *
       01 limpa                        pic x(10) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 flag-empresa                 pic x(01) value "N".
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 linha                        pic 9(03) comp-5 value 0.
       01 coluna                       pic 9(04) comp-5 value 1.
       01 tamanho                      pic 9(04) comp-5 value 78.
       01 pagina                       pic 9(03) value 0.
       01 tracos-c                     pic x(78) value all "Ä".
       01 tracos-i                     pic x(80) value all "-".
       01 total                        pic 9(05) value 0.
      *
       01 buffer2.
          02 filler                    pic 9(04) occurs 2000.
      *
       01 campos.
          02 sele-ord                  pic 9(01) value 0.
          02 sele-tipo                 pic 9(01) value 0.
          02 sele-rateio               pic x(01) value spaces.
          02 sele-rateio-disp          pic x(05) value spaces.
          02 sele-device               pic 9(01) value 0.
      *
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       01 cab-abav.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(01) value x"0e".
          02 filler                    pic x(08) value "ABAV/SP ".
          02 filler                    pic x(01) value x"14".
          02 filler                    pic x(01) value x"0f".
          02 filler                    pic x(65) value
          "Associcao Brasileira de Agencias de Viagens de Sao Paulo - BA
      -   "LCAO".
          02 filler                    pic x(01) value x"12".
      *
       01 cab-prog.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(19) value
          "Relacao de Usuarios".
          02 filler                    pic x(34) value spaces.
          02 cab-data                  pic x(08) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Pag. ".
          02 cab-pagina                pic 9(04) value 0.
      *
       01 cab-01.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(06) value "Codigo".
          02 filler                    pic x(06) value spaces.
          02 filler                    pic x(12) value "Razao Social".
          02 filler                    pic x(46) value spaces.
          02 filler                    pic x(06) value "Rateio".
          02 filler                    pic x(01) value spaces.
      *
       01 cab-02.
          02 filler                    pic x(02) value spaces.
          02 cab-codigo                pic 9(05) value 0.
          02 filler                    pic x(06) value spaces.
          02 cab-razao-social          pic x(40) value spaces.
          02 filler                    pic x(21) value spaces.
          02 cab-rateio                pic x(01) value spaces.
          02 filler                    pic x(03) value spaces.
      *
       01 cab-03.
          02 filler                    pic x(11) value "Empresa..:".
          02 cab-razao-social-a        pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(11) value "Codigo...:".
          02 cab-codigo-a              pic 9(05) value 0 
             blank when zero.
      *
       01 cab-04.
          02 filler                    pic x(11) value "Titular..:".
          02 cab-Titular-a             pic x(40) value spaces.
      *
       01 cab-05.
          02 filler                    pic x(11) value "Endereco.:".
          02 cab-endereco-a            pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(06) value "U.F.:".
          02 cab-uf-a                  pic x(02) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(08) value "C.E.P.:".
          02 cab-cep-a                 pic 9(05)b9(03) value 0
             blank when zero.
      *
       01 cab-06.
          02 filler                    pic x(11) value "Cidade...:".
          02 cab-cidade-a              pic x(15) value spaces.
          02 filler                    pic x(26) value spaces.
          02 filler                    pic x(11) value "D.D.D....:".
          02 cab-ddd-a                 pic 9(04) value 0
             blank when zero.
      *
       01 cab-07.
          02 filler                    pic x(11) value "Fone.....:".
          02 cab-telefone-a-1          pic x(08) value spaces.
          02 filler                    pic x(08) value spaces.
          02 cab-telefone-a-2          pic x(08) value spaces.
          02 filler                    pic x(17) value spaces.
          02 filler                    pic x(11) value "Telex....:".
          02 cab-telex-a               pic x(09) value spaces.
      *
       01 cab-08.
          02 filler                    pic x(11) value "Fax......:".
          02 cab-fax-a                 pic x(08) value spaces.
          02 filler                    pic x(08) value spaces.
          02 filler                    pic x(11) value "Ramal....:".
          02 cab-ramal-a-1             pic 9(03) value 0
          blank when zero.
          02 filler                    pic x(02) value spaces.
          02 cab-ramal-a-2             pic 9(03) value 0
          blank when zero.      
          02 filler                    pic x(06) value spaces.
          02 filler                    pic x(11) value "Rateio...:".
          02 cab-rateio-a              pic x(01) value spaces.
      *
       01 cab-tot.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(05) value "Total".
          02 filler                    pic x(65) value spaces.
          02 cab-total                 pic 9(05) value 0.
          02 filler                    pic x(01) value spaces.
      *
       copy workgen.lib.
      * 
       linkage section.
      *
       01 param-menu.
          02 param-usr                 pic x(10).
          02 param-senha               pic x(10).
          02 param-prioridade          pic 9(01).
          02 param-data                pic 9(05).
          02 param-impress             pic x(12).
      *
       screen section.
      *
       01 tela-01.
          02 line 13 column 30 foreground-color 06 background-color 01
             highlight value "Consulta/Relacao".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "Ordenamento...:".
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "Tipo..........:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Rateio........:".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "Device........:".
      *
       01 tela-02.
          02 line 19 column 05 foreground-color 02 background-color 03
             highlight pic x(41) from spaces.
          02 line 19 column 08 foreground-color 02 background-color 03
             highlight value "1".
          02 line 19 column 09 foreground-color 05 background-color 03
             value "-Codigo".
          02 line 19 column 25 foreground-color 02 background-color 03
             highlight value "2".
          02 line 19 column 26 foreground-color 05 background-color 03
             value "-Razao Social".
      *
       01 tela-03.
          02 line 19 column 05 foreground-color 02 background-color 03
             highlight pic x(41) from spaces.
          02 line 19 column 08 foreground-color 02 background-color 03
             highlight value "S".
          02 line 19 column 09 foreground-color 05 background-color 03
             value " - Sim".
          02 line 19 column 25 foreground-color 02 background-color 03
             highlight value "N".
          02 line 19 column 26 foreground-color 05 background-color 03
             value " - Nao".
      *
       01 tela-04.
          02 line 19 column 05 foreground-color 02 background-color 03
             highlight pic x(41) from spaces.
          02 line 19 column 08 foreground-color 02 background-color 03
             highlight value "1".
          02 line 19 column 09 foreground-color 05 background-color 03
             value "-Console".
          02 line 19 column 25 foreground-color 02 background-color 03
             highlight value "2".
          02 line 19 column 26 foreground-color 05 background-color 03
             value "-Impressora".
      *
       01 tela-05.
          02 line 23 column 02 foreground-color 05 background-color 03
             pic x(78) from spaces.
          02 line 23 column 02 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 23 column 09 foreground-color 02 background-color 03
             highlight value "C".
          02 line 23 column 10 foreground-color 05 background-color 03
             value ")ontinuar   (".
          02 line 23 column 23 foreground-color 02 background-color 03
             highlight value "F".
          02 line 23 column 24 foreground-color 05 background-color 03
             value ")inalizar".
      *
       01 tela-06.
          02 line 23 column 02 foreground-color 05 background-color 03
             pic x(78) from spaces.
          02 line 23 column 02 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 23 column 09 foreground-color 02 background-color 03
             highlight value "I".
          02 line 23 column 10 foreground-color 05 background-color 03
             value ") para interromper".
      *
       01 tela-07.
          02 line 23 column 02 foreground-color 05 background-color 03
             pic x(78) from spaces.
          02 line 23 column 02 foreground-color 05 background-color 03
             value "Tecle <Enter>".
      *
       01 tela-08.
          02 line 19 column 05 foreground-color 05 background-color 03
             pic x(41) from spaces.
          02 line 19 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 19 column 12 foreground-color 02 background-color 03
             highlight value "C".
          02 line 19 column 13 foreground-color 05 background-color 03
             value ")ontinuar   (".
          02 line 19 column 26 foreground-color 02 background-color 03
             highlight value "F".
          02 line 19 column 27 foreground-color 05 background-color 03
             value ")inalizar".
      *
       01 tela-09.
          02 line 19 column 05 foreground-color 05 background-color 03
             pic x(41) from spaces.
          02 line 19 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 19 column 12 foreground-color 02 background-color 03
             highlight value "I".
          02 line 19 column 13 foreground-color 05 background-color 03
             value ") para interromper".
      *
       01 tela-10.
          02 line 19 column 05 foreground-color 02 background-color 03
             highlight pic x(41) from spaces.
          02 line 19 column 08 foreground-color 02 background-color 03
             highlight value "1".
          02 line 19 column 09 foreground-color 05 background-color 03
             value "-Sintetico".
          02 line 19 column 25 foreground-color 02 background-color 03
             highlight value "2".
          02 line 19 column 26 foreground-color 05 background-color 03
             value "-Analitico".
      *
       01 tela-mensagem-cad.
          02 line 19 column 05 foreground-color 07 background-color 01
             highlight pic x(41) from mensagem.
      *
       01 tela-erro-cad.
          02 line 19 column 05 beep reverse-video pic x(41) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 19 column 05 foreground-color 05 background-color 01
             pic x(41) from spaces.
      *
       copy scrgen.lib.
      *
       procedure division using param-menu.
      *
       lab-00.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-save-buffer.
           display tela-cabec.
           move 03 to box-col.
           move 11 to box-lin.
           move 45 to box-col-f.
           move 19 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-01.
      *
       lab-01.
           display tela-limpa-cad.
           perform sec-selecao.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-rest-buffer.
      *
       lab-fim.
           exit program.
      *
      ************************
      *                      *
      *    R o t i n a s     *
      *                      *
      ************************
      *      
       rotinas section.
      *
       rot-le-ab02.
           move 0 to erro.
           read arqab02 invalid key move 1 to erro.
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab02.
      *
       rot-le-ab02-1.
           move 0 to erro.
           read arqab02 key ab02-chave-1 invalid key move 1 to erro.
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab02-1.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqab02 previous at end move 1 to erro.
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqab02 next at end move 1 to erro.
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-ab01.
           move 0 to erro.
           if ab01-stat = "F"
              open i-o arqab01
              if ab01-status not = "00"
                 move 
                 " Erro de abertura no ARQAB01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ab01-stat
               end-if
           end-if.
      *
       rot-close-ab01.
           if ab01-stat = "A"
              close arqab01
              move "F" to ab01-stat
           end-if.
      *
       err-leitura-ab01.
           move " Erro de leitura - ARQAB01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-ab01.
           move 0 to erro.
           read arqab01 invalid key move 1 to erro.
           if ab01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab01
           end-if.
      *
       rot-open-ab02.
           move 0 to erro.
           if ab02-stat = "F"
              open i-o arqab02
              if ab02-status not = "00"
                 move 
                 " Erro de abertura no ARQAB02A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ab02-stat
               end-if
           end-if.
      *
       rot-close-ab02.
           if ab02-stat = "A"
              close arqab02
              move "F" to ab02-stat
           end-if.
      *
       rot-open-ab04.
           move 0 to erro.
           if ab04-stat = "F"
              open i-o arqab04
              if ab04-status not = "00"
                 move 
                 " Erro de abertura no ARQAB04A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ab04-stat
               end-if
           end-if.
      *
       rot-close-ab04.
           if ab04-stat = "A"
              close arqab04
              move "F" to ab04-stat
           end-if.
      *
       err-leitura-ab04.
           move " Erro de leitura - ARQAB04A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-ab04.
           move 0 to erro.
           read arqab04 invalid key move 1 to erro.
           if ab04-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab04
           end-if.
      *
       rot-open-imp.
           move 0 to erro.
           move param-impress to impress.
           move zeros to imp-status
           if imp-stat = "F"
              open output arqimp
              if imp-status not = "00"
                 move " Erro de impressao - Tecle <Enter>" to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
              else
                 move "A" to imp-stat
              end-if
           end-if.
      *
       rot-close-imp.
           if imp-stat = "A"
              close arqimp 
              unlock arqimp
              move "F" to imp-stat
           end-if.
      *
       rot-erro-leitura-ab02.
           move " Erro de leitura - ARQAB02A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-interrompe-console.
           call "C_Readkey".
           move return-code to campo-kbd.
           if kbd2 = 73 or 105
              display tela-05
              perform until kbd2 = 67 or 99 or 70 or 102
                      perform rot-keypress
              end-perform
             if kbd2 = 70 or 102
                move "F" to resposta
             else
                display tela-06
             end-if
           end-if.
      *
       rot-interrompe-impressora.
           call "C_Readkey".
           move return-code to campo-kbd.
           if kbd2 = 73 or 105
              display tela-08
              perform until kbd2 = 67 or 99 or 70 or 102
                      perform rot-keypress
              end-perform
             if kbd2 = 70 or 102
                move "F" to resposta
             else
                display tela-09
             end-if
           end-if.
      *
       rot-cabec.
           move param-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move 7 to linha.
           add 1 to pagina
           move pagina to cab-pagina
           if pagina = 1
              write reg-imp from cab-abav
              write reg-imp from cab-prog after 1 line
           else
              write reg-imp from cab-abav after page
              write reg-imp from cab-prog after 2 lines
           end-if.
           if sele-tipo = 1
              write reg-imp from tracos-i after 1 line
              write reg-imp from cab-01 after 1 line
              write reg-imp from tracos-i after 1 line
              write reg-imp from spaces
          else
              write reg-imp from spaces after 1 line
              write reg-imp from tracos-i after 1 line
          end-if.
      *
       rot-move.
           move ab02-codigo to cab-codigo cab-codigo-a.
           move ab02-razao-social-a to cab-razao-social 
                                       cab-razao-social-a.
           move ab02-rateio to cab-rateio cab-rateio-a.
           if sele-tipo = 2
              perform rot-le-titular
              move ab04-nome-a to cab-titular-a
              perform rot-le-associado
              move ab01-endereco to cab-endereco-a
              move ab01-cep to cab-cep-a
              move ab01-uf to cab-uf-a
              move ab01-cidade to cab-cidade-a
              move ab01-ddd to cab-ddd-a
              move ab01-telefone (01) to cab-telefone-a-1
              move ab01-telefone (02) to cab-telefone-a-2
              move ab01-telex to cab-telex-a
              move ab01-fax to cab-fax-a
              move ab01-ramal (01) to cab-ramal-a-1
              move ab01-ramal (02) to cab-ramal-a-2
           end-if.
           add 1 to total.
      *
       rot-le-titular.
          move low-value to ab04-chave.
          move ab02-codigo to ab04-codigo.
          move "A" to ab04-condicao.
          move 01 to ab04-titular.
          perform rot-le-ab04.
          if erro not = 0
             move spaces to ab04-nome-a
          end-if.
      *
       rot-le-associado.
          move low-value to ab01-chave.
          move ab02-codigo to ab01-codigo.
          move "A" to ab01-condicao.
          perform rot-le-ab01.
          if erro not = 0
             move spaces to ab01-razao-social-a
          end-if.
      *
       copy rotgen.lib.
      *
      ************************
      *                      *
      *       T e l a s      *
      *                      *
      ************************
      *
       telas section.
      *
       accept-resposta-cad.
           move spaces to resposta.
           accept resposta at 1740 with auto foreground-color 01
                                             background-color 01.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
       display-erro-usr.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
      *  Sequencia para dar Accept
      *
       acc-ord.
           accept sele-ord at 1422 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-tipo.
           accept sele-tipo at 1522 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-rateio.
           accept sele-rateio at 1622 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-device.
           accept sele-device at 1722 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-ord.
           display sele-ord at 1422 with foreground-color 15 
                   background-color 01.
      *
       dsp-tipo.
           display sele-tipo at 1522 with foreground-color 15 
                   background-color 01.
      *
       dsp-rateio.
           display sele-rateio-disp at 1622 with foreground-color 15 
                   background-color 01.
      *
       dsp-device.
           display sele-device at 1722 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-ord.
           display limpa at 1422 with foreground-color 15 
                   background-color 01.
      *
       lmp-tipo.
           display limpa at 1522 with foreground-color 15 
                   background-color 01.
      *
       lmp-rateio.
           display limpa at 1622 with foreground-color 15 
                   background-color 01.
      *
       lmp-device.
           display limpa at 1722 with foreground-color 15 
                   background-color 01.
      *
       sec-selecao section.
      *
       lab-sele-01.
           display tela-02.
           move 0 to sele-ord.
           perform lmp-ord.
           perform acc-ord.
           if escape-key = 1
              go to lab-sele-fim
           end-if.
           if sele-ord not = 1 and 2 
              go to lab-sele-01
           end-if.
      *
       lab-sele-02.
           display tela-10.
           move 0 to sele-tipo.
           perform lmp-tipo.
           perform acc-tipo.
           if escape-key = 1
              perform lmp-tipo
              go to lab-sele-01
           end-if.
           if sele-tipo not = 1 and 2
              go to lab-sele-02
           end-if.
      *
       lab-sele-03.
           display tela-03.
           move spaces to sele-rateio.
           perform lmp-rateio.
           perform acc-rateio.
           if escape-key = 1
              perform lmp-rateio
              go to lab-sele-02
           end-if.
           move sele-rateio to txt.
           perform rot-texto.
           if txt not = "S" and "N" and spaces
              go to lab-sele-03
           end-if.
           move txt to sele-rateio sele-rateio-disp.
           if sele-rateio = spaces
              move "Todos" to sele-rateio-disp
           end-if. 
           perform dsp-rateio.
           if sele-tipo = 2
              move 2 to sele-device
              perform dsp-device
              go to lab-sele-05
           end-if.
      *
       lab-sele-04.
           display tela-04.
           move 0 to sele-device.
           perform lmp-device.
           perform acc-device.
           if escape-key = 1
              perform lmp-device
              go to lab-sele-03
           end-if.
           if sele-device not = 1 and 2
              go to lab-sele-04
           end-if.
      *
       lab-sele-05.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              if sele-tipo = 1
                 go to lab-sele-04
              else
                 perform lmp-device
                 go to lab-sele-03
              end-if
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-ord thru lmp-device
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-05
              end-if
           end-if.
           display tela-limpa-cad.
           if sele-device = 1
              perform sec-consulta
           else
              perform sec-impressao
           end-if.
           perform lmp-ord thru lmp-device.
           go to lab-sele-01.
      *
       lab-sele-fim.
           perform lmp-ord thru lmp-device.
           exit.
      *
       sec-consulta section.
       sec-cns-00.
           move 01 to box-col.
           move 03 to box-lin.
           move 78 to box-col-f.
           move 22 to box-lin-f. 
           call "C_Savescr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer2.
           display tela-cabec.
           move spaces to box-borda.
           move 00 to box-cor-f.
           move 15 to box-cor-p.
           move spaces to box-fundo.
           move "N" to box-sombra.
           perform rot-box.
           perform rot-open-ab02.
           if erro not = 0
              go to lab-cns-fim
           end-if.
           move 99 to linha.
           move 0 to total.
           evaluate true
                  when sele-ord = 1
                       move low-values to ab02-chave
                       start arqab02 key is not less ab02-chave
                  when sele-ord = 2
                       move low-values to ab02-chave-1
                       start arqab02 key is not less ab02-chave-1
           end-evaluate.
      *
       lab-cns-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-cns-fim
           end-if.
           if ab02-chave = high-values
              go to lab-cns-01
           end-if.
           perform rot-interrompe-console.
           if resposta = "F"
              move 27 to kbd2
              go to lab-cns-fim
           end-if.
           if sele-rateio not = spaces
              if sele-rateio not = ab02-rateio
                 go to lab-cns-01
              end-if
           end-if.
           if linha > 19
              if linha not = 99
                 display tela-07
                 perform rot-keypress
                 if kbd2 = 27
                    go to lab-cns-fim
                 end-if
              end-if
              move 4 to linha
              perform rot-box
              display tela-06
              call "C_Writexy" using by value coluna
                                     by value linha
                                     by value tamanho
                                     by value box-cor-f
                                     by value box-cor-p
                                     by reference cab-01
              add 1 to linha
              call "C_Writexy" using by value coluna
                                     by value linha
                                     by value tamanho
                                     by value box-cor-f
                                     by value box-cor-p
                                     by reference tracos-c
              add 1 to linha
           end-if.
           perform rot-move.
           call "C_Writexy" using by value coluna
                                  by value linha
                                  by value tamanho
                                  by value box-cor-f
                                  by value box-cor-p
                                  by reference cab-02.
           add 1 to linha.
           go to lab-cns-01.
      * 
       lab-cns-fim.
           if kbd2 not = 27
              call "C_Writexy" using by value coluna
                                     by value linha
                                     by value tamanho
                                     by value box-cor-f
                                     by value box-cor-p
                                     by reference tracos-c
              add 1 to linha
              move total to cab-total
              call "C_Writexy" using by value coluna
                                     by value linha
                                     by value tamanho
                                     by value box-cor-f
                                     by value box-cor-p
                                     by reference cab-tot
              perform rot-close-ab02
              display tela-07
              perform rot-keypress
           end-if.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer2.
           exit.
      *
       sec-impressao section.
       lab-imp-00.
           perform rot-open-ab01.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-ab02.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-ab04.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           move 99 to linha.
           move 0 to pagina total.
           evaluate true
                  when sele-ord = 1
                       move low-values to ab02-chave
                       start arqab02 key is not less ab02-chave
                  when sele-ord = 2
                       move low-values to ab02-chave-1
                       start arqab02 key is not less ab02-chave-1
           end-evaluate.
           display tela-09.
      *
       lab-imp-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if ab02-chave = high-values
              go to lab-imp-01
           end-if.
           perform rot-interrompe-impressora.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           if sele-rateio not = spaces
              if sele-rateio not = ab02-rateio
                 go to lab-imp-01
              end-if
           end-if.
           if linha > 56
              perform rot-cabec
           end-if.
           perform rot-move.
           if sele-tipo = 1
              write reg-imp from cab-02 after 1 line
              add 1 to linha
           else
              write reg-imp from cab-03 after 1 line
              write reg-imp from cab-04 after 1 line
              write reg-imp from cab-05 after 1 line
              write reg-imp from cab-06 after 1 line
              write reg-imp from cab-07 after 1 line
              write reg-imp from cab-08 after 1 line
              write reg-imp from tracos-i after 1 line
              add 7 to linha
           end-if.
           go to lab-imp-01.
      * 
       lab-imp-fim.
           if kbd2 not = 27
              move total to cab-total
              if sele-tipo = 1
                 write reg-imp from tracos-i after 1 line
              end-if
              write reg-imp from cab-tot after 1 line
              write reg-imp from tracos-i after 1 line
           end-if.
           if pagina not = 0
              write reg-imp from spaces after page
           end-if.
           perform rot-close-imp.
           perform rot-close-ab04.
           perform rot-close-ab02.
           perform rot-close-ab01.
           exit.
      *