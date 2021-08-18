      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  PGCE05       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Relacao/Consulta de Grupos :                               *
      *                                                             *
      *  Data da ultima alteracao:    23/04/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgce05.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqce01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ce01-chave
                  alternate record key is ce01-chave-1 with duplicates
                  file status is ce01-status.
      *
           select arqimp assign to disk
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
       data division.
       file section.
      *    
       copy fdce01.lib.
      *
       fd arqimp
      
       label record is standard
       value of file-id is impress
       data record is reg-imp.

       01 reg-imp                      pic x(300).
      *
       working-storage section.
      *
       01 ce01-status                  pic x(02) value "00".
       01 ce01-stat                    pic x(01) value "F".
      *
       01 nome-arq-ce01.
          02 ce01-dir                  pic x(03) value "CE2".
          02 filler                    pic x(01) value "\".
          02 ce01-nome                 pic x(08) value "ARQCE01A".
          02 filler                    pic x(01) value ".".
          02 ce01-ext                  pic x(03) value "DAT".
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGCE05".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(10) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 flag-empresa                 pic x(01) value "N".
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 linha                        pic 9(03) comp-5 value 0.
       01 linha-detalhe                pic x(78) value spaces.
       01 coluna                       pic 9(04) comp-5 value 1.
       01 tamanho                      pic 9(04) comp-5 value 78.
       01 pagina                       pic 9(03) value 0.
       01 tracos-c                     pic x(78) value all "Ä".
       01 tracos-i                     pic x(78) value all "-".
       01 total                        pic 9(05) value 0.
      *
       01 buffer2.
          02 filler                    pic 9(04) occurs 2000.
      *
       01 campos.
          02 sele-ord                  pic 9(01) value 0.
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
          02 filler                    pic x(08) value "DISPASA ".
          02 filler                    pic x(01) value x"14".
          02 filler                    pic x(01) value x"0f".
          02 filler                    pic x(23) value
          "Dispasa Plasticos LTDA.".
          02 filler                    pic x(01) value x"12".
      *
       01 cab-prog.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(19) value
          "Relacao de Grupos".
          02 filler                    pic x(36) value spaces.
          02 cab-data                  pic x(08) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Pag. ".
          02 cab-pagina                pic 9(04) value 0.
      *
       01 cab-01.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(05) value "Grupo".
          02 filler                    pic x(07) value spaces.
          02 filler                    pic x(09) value "Descricao".
          02 filler                    pic x(39) value spaces.
          02 filler                    pic x(07) value "Unidade".
          02 filler                    pic x(01) value spaces.
      *
       01 cab-02.
          02 filler                    pic x(01) value spaces.
          02 cab-grupo                 pic 9(05) value 0.
          02 filler                    pic x(07) value spaces.
          02 cab-descricao             pic x(40) value spaces.
          02 filler                    pic x(08) value spaces.
          02 cab-unidade               pic x(10) value spaces.
          02 filler                    pic x(07) value spaces.
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
          02 line 13 column 30 foreground-color 07 background-color 04
             highlight value "Consulta/Relacao".
          02 line 14 column 06 foreground-color 06 background-color 04
             value "Ordenamento...:".
          02 line 15 column 06 foreground-color 06 background-color 04
             value "Device........:".
      *
       01 tela-02.
          02 line 17 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 17 column 08 foreground-color 07 background-color 02
             highlight value "1".
          02 line 17 column 09 foreground-color 01 background-color 02
             value "-Codigo".
          02 line 17 column 25 foreground-color 07 background-color 02
             highlight value "2".
          02 line 17 column 26 foreground-color 01 background-color 02
             value "-Descricao".
      *
       01 tela-04.
          02 line 17 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 17 column 08 foreground-color 07 background-color 02
             highlight value "1".
          02 line 17 column 09 foreground-color 01 background-color 02
             value "-Console".
          02 line 17 column 25 foreground-color 07 background-color 02
             highlight value "2".
          02 line 17 column 26 foreground-color 01 background-color 02
             value "-Impressora".
      *
       01 tela-05.
          02 line 23 column 02 foreground-color 01 background-color 02
             pic x(78) from spaces.
          02 line 23 column 02 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 23 column 09 foreground-color 07 background-color 02
             highlight value "C".
          02 line 23 column 10 foreground-color 01 background-color 02
             value ")ontinuar   (".
          02 line 23 column 23 foreground-color 07 background-color 02
             highlight value "F".
          02 line 23 column 24 foreground-color 01 background-color 02
             value ")inalizar".
      *
       01 tela-06.
          02 line 23 column 02 foreground-color 01 background-color 02
             pic x(78) from spaces.
          02 line 23 column 02 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 23 column 09 foreground-color 07 background-color 02
             highlight value "I".
          02 line 23 column 10 foreground-color 01 background-color 02
             value ") para interromper".
      *
       01 tela-07.
          02 line 23 column 02 foreground-color 01 background-color 02
             pic x(78) from spaces.
          02 line 23 column 02 foreground-color 01 background-color 02
             value "Tecle <Enter>".
      *
       01 tela-08.
          02 line 17 column 05 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 17 column 05 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 17 column 12 foreground-color 07 background-color 02
             highlight value "C".
          02 line 17 column 13 foreground-color 01 background-color 02
             value ")ontinuar   (".
          02 line 17 column 26 foreground-color 07 background-color 02
             highlight value "F".
          02 line 17 column 27 foreground-color 01 background-color 02
             value ")inalizar".
      *
       01 tela-09.
          02 line 17 column 05 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 17 column 05 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 17 column 12 foreground-color 07 background-color 02
             highlight value "I".
          02 line 17 column 13 foreground-color 01 background-color 02
             value ") para interromper".
      *
       01 tela-10.
          02 line 17 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 17 column 08 foreground-color 07 background-color 02
             highlight value "1".
          02 line 17 column 09 foreground-color 01 background-color 02
             value "-Sintetico".
          02 line 17 column 25 foreground-color 07 background-color 02
             highlight value "2".
          02 line 17 column 26 foreground-color 01 background-color 02
             value "-Analitico".
      *
       01 tela-mensagem-cad.
          02 line 17 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from mensagem.
      *
       01 tela-erro-cad.
          02 line 17 column 05 beep reverse-video pic x(41) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 17 column 05 foreground-color 04 background-color 04
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
           move 17 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
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
       rot-open-ce01.
           move 0 to erro.
           if ce01-stat = "F"
              open i-o arqce01
              if ce01-status not = "00"
                 move 
                 " Erro de abertura no ARQCE01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ce01-stat
               end-if
           end-if.
      *
       rot-close-ce01.
           if ce01-stat = "A"
              close arqce01
              move "F" to ce01-stat
           end-if.
      *
       rot-erro-leitura-ce01.
           move " Erro de leitura - ARQCE01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqce01 next at end move 1 to erro.
           if ce01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
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
           write reg-imp from tracos-i after 1 line.
           write reg-imp from cab-01 after 1 line.
           write reg-imp from tracos-i after 1 line.
           write reg-imp from spaces.
      *
       rot-move.
          move ce01-grupo to cab-grupo.
          move ce01-descricao-a to cab-descricao.
          move ce01-unidade to cab-unidade.
      *
       rot-print.
           if linha not > 23
              call "C_Writexy" using by value coluna
                                     by value linha
                                     by value tamanho
                                     by value box-cor-f
                                     by value box-cor-p
                                     by reference linha-detalhe
              add 1 to linha
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
           accept resposta at 1640 with auto foreground-color 04
                                             background-color 04.
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
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-device.
           accept sele-device at 1522 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-ord.
           display sele-ord at 1422 with foreground-color 15 
                   background-color 04.
      *
       dsp-device.
           display sele-device at 1522 with foreground-color 15 
                   background-color 04.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-ord.
           display limpa at 1422 with foreground-color 15 
                   background-color 04.
      *
       lmp-device.
           display limpa at 1522 with foreground-color 15 
                   background-color 04.
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
           display tela-04.
           move 0 to sele-device.
           perform lmp-device.
           perform acc-device.
           if escape-key = 1
              perform lmp-device
              go to lab-sele-01
           end-if.
           if sele-device not = 1 and 2
              go to lab-sele-02
           end-if.
      *
       lab-sele-03.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-02
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-ord thru lmp-device
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-03
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
           perform rot-open-ce01.
           if erro not = 0
              go to lab-cns-fim
           end-if.
           move 99 to linha.
           move 0 to total.
           evaluate true
                  when sele-ord = 1
                       move low-values to ce01-chave
                       start arqce01 key is not less ce01-chave
                  when sele-ord = 2
                       move low-values to ce01-chave-1
                       start arqce01 key is not less ce01-chave-1
           end-evaluate.
      *
       lab-cns-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-cns-fim
           end-if.
           if ce01-chave = high-values
              go to lab-cns-01
           end-if.
           perform rot-interrompe-console.
           if resposta = "F"
              move 27 to kbd2
              go to lab-cns-fim
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
              move cab-01 to linha-detalhe
              perform rot-print
              move tracos-c to linha-detalhe
              perform rot-print
           end-if.
           perform rot-move.
           move cab-02 to linha-detalhe.
           perform rot-print.
           go to lab-cns-01.
      * 
       lab-cns-fim.
           if kbd2 not = 27
              display tela-07
              perform rot-keypress
           end-if.
           perform rot-close-ce01.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer2.
           exit.
      *
      *
       sec-impressao section.
           perform rot-open-ce01.
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
                       move low-values to ce01-chave
                       start arqce01 key is not less ce01-chave
                  when sele-ord = 2
                       move low-values to ce01-chave-1
                       start arqce01 key is not less ce01-chave-1
           end-evaluate.
           display tela-09.
      *
       lab-imp-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if ce01-chave = high-values
              go to lab-imp-01
           end-if.
           perform rot-interrompe-impressora.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           if linha > 56
              perform rot-cabec
           end-if.
           perform rot-move.
           write reg-imp from cab-02 after 1 line.
           add 1 to linha.
           go to lab-imp-01.
      * 
       lab-imp-fim.
           if pagina not = 0
              write reg-imp from spaces after page
           end-if.
           perform rot-close-imp.
           perform rot-close-ce01.
           exit.
      *