      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGAB05       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Relacao do cadastro (TKTS):                                *
      *                                                             *
      *  Data da ultima alteracao:    06/12/93     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgab05.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqab03 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab03-chave
                  alternate record key is ab03-chave-1
                  file status is ab03-status.
      *
           select arqimp assign to disk
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
       data division.
       file section.
      *    
       copy fdab03.lib.
      *
       fd arqimp
      
       label record is standard
       value of file-id is impress
       data record is reg-imp.

       01 reg-imp                      pic x(300).
      *
       working-storage section.
      *
       01 ab03-status                  pic x(02) value "00".
       01 ab03-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab03.
          02 ab03-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab03-nome                 pic x(08) value "ARQAB03A".
          02 filler                    pic x(01) value ".".
          02 ab03-ext                  pic x(03) value "DAT".
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGAB05".
          02 cb-versao                 pic x(06) value "v1.00 ".
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
       01 tracos-i                     pic x(78) value all "-".
       01 total-tkt                    pic s9(05) value 0.
       01 todos                        pic x(05) value "Todos".
      *
       01 buffer2.
          02 filler                    pic 9(04) occurs 2000.
      *
       01 campos.
          02 sele-ord                  pic 9(01) value 0.
          02 sele-data-i               pic 9(06) value 0.
          02 sele-data-disp-i          pic x(08) value spaces.
          02 sele-data-f               pic 9(06) value 0.
          02 sele-data-disp-f          pic x(08) value spaces.
          02 sele-codigo               pic 9(05) value 0.
          02 sele-aerop                pic 9(01) value 0.
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
          02 filler                    pic x(25) value
          "Relacao de TKTS entregues".
          02 filler                    pic x(28) value spaces.
          02 cab-data                  pic x(08) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Pag. ".
          02 cab-pagina                pic 9(04) value 0.
      *
       01 cab-01.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(04) value "Data".
          02 filler                    pic x(07) value spaces.
          02 filler                    pic x(06) value "Codigo".
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(11) value "Consorciado".
          02 filler                    pic x(39) value spaces.
          02 filler                    pic x(04) value "TKTS".
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(01) value "A".
          02 filler                    pic x(01) value spaces.
      *
       01 cab-02.
          02 filler                    pic x(02) value spaces.
          02 cab-data-i                pic x(08) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-codigo                pic 9(05) value 0.
          02 filler                    pic x(03) value spaces.
          02 cab-descricao             pic x(40) value spaces.
          02 filler                    pic x(10) value spaces.
          02 cab-tkt                   pic 9(04) value 0.
          02 filler                    pic x(01) value spaces.
          02 cab-aerop                 pic 9(01) value 0.
          02 filler                    pic x(01) value spaces.
      *
       01 cab-tot.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(06) value "Totais".
          02 filler                    pic x(62) value spaces.
          02 cab-total-tkt             pic 9(05) value 0.
          02 filler                    pic x(02) value spaces.
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
          02 line 13 column 30 foreground-color 07 background-color 06
             highlight value "Consulta\Relacao".
          02 line 14 column 06 foreground-color 02 background-color 06
             value "Ordenamento...:".
          02 line 15 column 06 foreground-color 02 background-color 06
             value "Data Inicial..:".
          02 line 16 column 06 foreground-color 02 background-color 06
             value "Data Final....:".
          02 line 17 column 06 foreground-color 02 background-color 06
             value "Codigo........:".
          02 line 18 column 06 foreground-color 02 background-color 06
             value "Aeroporto.....:".
          02 line 19 column 06 foreground-color 02 background-color 06
             value "Device........:".
      *
       01 tela-02.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 21 column 08 foreground-color 07 background-color 02
             highlight value "1".
          02 line 21 column 09 foreground-color 01 background-color 02
             value "-Codigo".
          02 line 21 column 25 foreground-color 07 background-color 02
             highlight value "2".
          02 line 21 column 26 foreground-color 01 background-color 02
             value "-Descricao".
      *
       01 tela-03.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 21 column 07 foreground-color 07 background-color 02
             highlight value "1".
          02 line 21 column 08 foreground-color 01 background-color 02
             value "-Congonhas (CHG)".
          02 line 21 column 26 foreground-color 07 background-color 02
             highlight value "2".
          02 line 21 column 27 foreground-color 01 background-color 02
             value "-Guarulhos (GRU)".
      *
       01 tela-04.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 21 column 08 foreground-color 07 background-color 02
             highlight value "1".
          02 line 21 column 09 foreground-color 01 background-color 02
             value "-Console".
          02 line 21 column 25 foreground-color 07 background-color 02
             highlight value "2".
          02 line 21 column 26 foreground-color 01 background-color 02
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
          02 line 21 column 05 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 21 column 05 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 21 column 12 foreground-color 07 background-color 02
             highlight value "C".
          02 line 21 column 13 foreground-color 01 background-color 02
             value ")ontinuar   (".
          02 line 21 column 26 foreground-color 07 background-color 02
             highlight value "F".
          02 line 21 column 27 foreground-color 01 background-color 02
             value ")inalizar".
      *
       01 tela-09.
          02 line 21 column 05 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 21 column 05 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 21 column 12 foreground-color 07 background-color 02
             highlight value "I".
          02 line 21 column 13 foreground-color 01 background-color 02
             value ") para interromper".
      *
       01 tela-mensagem-cad.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from mensagem.
      *
       01 tela-erro-cad.
          02 line 21 column 05 beep reverse-video pic x(41) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 21 column 05 foreground-color 06 background-color 06
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
           call "C_Savescr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer.
           display tela-cabec.
           move 03 to box-col.
           move 11 to box-lin.
           move 45 to box-col-f.
           move 21 to box-lin-f.
           move "3" to box-borda.
           move 06 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           call "C_Box" using by value box-col
                              by value box-lin
                              by value box-col-f
                              by value box-lin-f
                              by value box-cor-f
                              by value box-cor-p
                              by reference box-atrib.
           display tela-01.
       lab-01.
           display tela-limpa-cad.
           perform sec-selecao.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer.
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
       rot-le-ab03.
           move 0 to erro.
           read arqab03 invalid key move 1 to erro.
           if ab03-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab03.
      *
       rot-le-ab03-1.
           move 0 to erro.
           read arqab03 key ab03-chave-1 invalid key move 1 to erro.
           if ab03-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab03-1.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqab03 previous at end move 1 to erro.
           if ab03-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqab03 next at end move 1 to erro.
           if ab03-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-ab03.
           move 0 to erro.
           if ab03-stat = "F"
              open input arqab03
              if ab03-status not = "00"
                 move 
                 " Erro de abertura no ARQAB03A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ab03-stat
               end-if
           end-if.
      *
       rot-close-ab03.
           if ab03-stat = "A"
              close arqab03
              move "F" to ab03-stat
           end-if.
      *
       rot-open-imp.
           move 0 to erro.
           move param-impress to impress.
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
       rot-erro-leitura-ab03.
           move " Erro de leitura - ARQAB03A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-data-i.
           move " Data invalida - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-data-m.
           move " Data final menor que data inicial" 
           to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
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
           end-if.           write reg-imp from tracos-i after 1 line.
           write reg-imp from cab-01 after 1 line.
           write reg-imp from tracos-i after 1 line.
           write reg-imp from spaces.
      *
       rot-move.
           move ab03-codigo to cab-codigo.
           move ab03-descricao-a to cab-descricao.
           move ab03-data-i in ab03-chave to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data-i.
           move ab03-tkt to cab-tkt.
           move ab03-aerop in ab03-chave to cab-aerop.
           add ab03-tkt to total-tkt.
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
           accept resposta at 1930 with auto foreground-color 06
                                             background-color 06.
           accept escape-key from escape.
           move resposta to txt.
           call "C_Text" using by reference campo-txt.
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
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-data-i.
           accept sele-data-i at 1522 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-data-f.
           accept sele-data-f at 1622 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-codigo.
           accept sele-codigo at 1722 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-aerop.
           accept sele-aerop at 1822 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-device.
           accept sele-device at 1922 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-ord.
           display sele-ord at 1422 with foreground-color 15 
                   background-color 06.
      *
       dsp-data-i.
           display sele-data-disp-i at 1522 with foreground-color 15 
                   background-color 06.
      *
       dsp-data-f.
           display sele-data-disp-f at 1622 with foreground-color 15 
                   background-color 06.
      *
       dsp-codigo.
           display todos at 1722 with foreground-color 15 
                   background-color 06.
      *
       dsp-aerop.
           display todos at 1822 with foreground-color 15 
                   background-color 06.
      *
       dsp-device.
           display sele-device at 1922 with foreground-color 15 
                   background-color 06.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-ord.
           display limpa at 1422 with foreground-color 15 
                   background-color 06.
      *
       lmp-data-i.
           display limpa at 1522 with foreground-color 15 
                   background-color 06.
      *
       lmp-data-f.
           display limpa at 1622 with foreground-color 15 
                   background-color 06.
      *
       lmp-codigo.
           display limpa at 1722 with foreground-color 15 
                   background-color 06.
      *
       lmp-aerop.
           display limpa at 1822 with foreground-color 15 
                   background-color 06.
      *
       lmp-device.
           display limpa at 1922 with foreground-color 15 
                   background-color 06.
      *
       sec-selecao section.
       lab-sele-00.
           display tela-02.
      *
       lab-sele-01.
           move 0 to sele-ord.
           perform lmp-ord.
           perform acc-ord.
           if escape-key = 1
              go to lab-sele-fim
           end-if.
           if sele-ord not = 1 and 2
              go to lab-sele-01
           end-if.
           display tela-limpa-cad.
      *
       lab-sele-02.
           move 0 to sele-data-i.
           perform lmp-data-i.
           perform acc-data-i.
           if escape-key = 1 
              perform lmp-data-i
              display tela-02
              go to lab-sele-01
           end-if.
           if sele-data-i not = 0
              move sele-data-i to data-aux
              move dia-aux to dia-euro
              move mes-aux to mes-euro
              move ano-aux to ano-euro
              move 4 to opcao-data
              perform rot-data
              if return-code not = 0
                 perform rot-data-i
                go to lab-sele-02
              end-if
              move data-disp to sele-data-disp-i
              move dias-corr to sele-data-i
           else
              move 0 to sele-data-i
              move "Inicial" to sele-data-disp-i
           end-if.
           perform dsp-data-i.
      *
       lab-sele-03.
           move 0 to sele-data-f.
           perform lmp-data-f.
           perform acc-data-f.
           if escape-key = 1
              perform lmp-data-f
              go to lab-sele-02
           end-if.
           if sele-data-f not = 0
              move sele-data-f to data-aux
              move dia-aux to dia-euro
              move mes-aux to mes-euro
              move ano-aux to ano-euro
              move 4 to opcao-data
              perform rot-data
              if return-code not = 0
                 perform rot-data-i
                 go to lab-sele-03
              end-if
              move data-disp to sele-data-disp-f
              move dias-corr to sele-data-f
           else
              move 99999 to sele-data-f
              move "Final" to sele-data-disp-f
           end-if.
           if sele-data-i > sele-data-f
              perform rot-data-m
              go to lab-sele-03
           end-if.
           perform dsp-data-f.
      *
       lab-sele-04.
           move 0 to sele-codigo.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              perform lmp-codigo
              go to lab-sele-03
           end-if.
           if sele-codigo = 0
              perform dsp-codigo
          end-if.
      *
       lab-sele-05.
           display tela-03.
           move 0 to sele-aerop.
           perform lmp-aerop.
           perform acc-aerop.
           if escape-key = 1
              display tela-limpa-cad
              perform lmp-aerop
              go to lab-sele-04
           end-if.
           if sele-aerop > 2
              go to lab-sele-05
          end-if.
           if sele-aerop = 0
              perform dsp-aerop
          end-if.
      *
       lab-sele-06.
           display tela-04.
           move 0 to sele-device.
           perform lmp-device.
           perform acc-device.
           if escape-key = 1
              display tela-limpa-cad
              perform lmp-device
              go to lab-sele-05
           end-if.
           if sele-device not = 1 and 2
              go to lab-sele-06
           end-if.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-sele-07.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-06
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-ord thru lmp-device
              go to lab-sele-00
           else
              if resposta not = "S"
                 go to lab-sele-07
              end-if
           end-if.
           display tela-limpa-cad.
           if sele-device = 1
              perform sec-consulta
           else
              perform sec-impressao
           end-if.
           perform lmp-ord thru lmp-device.
           go to lab-sele-00.
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
           move 4 to box-cor-f.
           move 15 to box-cor-p.
           move spaces to box-fundo.
           move "N" to box-sombra.
           call "C_Box" using by value box-col
                              by value box-lin
                              by value box-col-f
                              by value box-lin-f
                              by value box-cor-f
                              by value box-cor-p
                              by reference box-atrib.
           perform rot-open-ab03.
           if erro not = 0
              go to lab-cns-fim
           end-if.
           move 99 to linha.
           move 0 to total-tkt.
           evaluate true
                  when sele-ord = 1
                       move low-values to ab03-chave
                       move sele-data-i to ab03-data-i in ab03-chave
                       start arqab03 key is not less ab03-chave
                  when sele-ord = 2
                       move low-values to ab03-chave-1
                       move sele-data-i to ab03-data-i in ab03-chave-1
                       start arqab03 key is not less ab03-chave-1
           end-evaluate.
      *
       lab-cns-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-cns-fim
           end-if.
           if ab03-chave = high-values
              go to lab-cns-01
           end-if.
           if sele-codigo not = 0
              if ab03-codigo not = sele-codigo
                 go to lab-cns-01
              end-if
           end-if.
           if sele-aerop not = 0
              if ab03-aerop in ab03-chave not = sele-aerop
                 go to lab-cns-01
              end-if
           end-if.
           perform rot-interrompe-console.
           if resposta = "F"
              move 27 to kbd2
              go to lab-cns-fim
           end-if.
           if ab03-data-i in ab03-chave > sele-data-f
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
              call "C_Box" using by value box-col
                                 by value box-lin
                                 by value box-col-f
                                 by value box-lin-f
                                 by value box-cor-f
                                 by value box-cor-p
                                 by reference box-atrib
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
              move total-tkt to cab-total-tkt
              call "C_Writexy" using by value coluna
                                     by value linha
                                     by value tamanho
                                     by value box-cor-f
                                     by value box-cor-p
                                     by reference cab-tot
              perform rot-close-ab03
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
           perform rot-open-ab03.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           move 99 to linha.
           move 0 to pagina total-tkt.
           evaluate true
                  when sele-ord = 1
                       move low-values to ab03-chave
                       move sele-data-i to ab03-data-i in ab03-chave
                       start arqab03 key is not less ab03-chave
                  when sele-ord = 2
                       move low-values to ab03-chave-1
                       move sele-data-i to ab03-data-i in ab03-chave-1
                       start arqab03 key is not less ab03-chave-1
           end-evaluate.
           display tela-09.
      *
       lab-imp-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if ab03-chave = high-values
              go to lab-imp-01
           end-if.
           if sele-codigo not = 0
              if ab03-codigo not = sele-codigo
                 go to lab-imp-01
              end-if
           end-if.
           if sele-aerop not = 0
              if ab03-aerop in ab03-chave not = sele-aerop
                 go to lab-imp-01
              end-if
           end-if.
           perform rot-interrompe-impressora.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           if ab03-data-i in ab03-chave > sele-data-f
              go to lab-imp-fim
           end-if.
           if linha > 55
              perform rot-cabec
           end-if.
           perform rot-move.
           write reg-imp from cab-02 after 1 line.
           add 1 to linha.
           go to lab-imp-01.
      * 
       lab-imp-fim.
           if kbd2 not = 27
              move total-tkt to cab-total-tkt
              write reg-imp from tracos-i after 1 line
              write reg-imp from cab-tot after 1 line
           end-if.
           if pagina not = 0
              write reg-imp from spaces after page
           end-if.
           perform rot-close-imp.
           perform rot-close-ab03.
           exit.
      *