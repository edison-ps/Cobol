      ***************************************************************
      *                                                             *
      *  A B A V / S P -  I N F O R M A T I C A   :::  PGAB08       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Quadro Estatistico :                                       *
      *                                                             *
      *  Data da ultima alteracao:    24/05/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgab08.
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
           select arqimp assign to printer
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
       data division.
       file section.
      *    
       copy fdab01.lib.
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
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGAB08".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(45) value spaces.
       01 limpa-aux                    pic x(15) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 flag-fim                     pic x(01) value "N".
       01 campo-dorme                  pic 9(04) comp-5 value 3.
       01 chave-ant                    pic x(06) value spaces.
       01 tracos                       pic x(78) value all "-".
      *
       01 campos.
          02 cap-assoc                 pic 9(04) value 0.
          02 cap-afil                  pic 9(04) value 0.
          02 cap-total                 pic 9(04) value 0.
          02 int-assoc                 pic 9(04) value 0.
          02 int-afil                  pic 9(04) value 0.
          02 int-total                 pic 9(04) value 0.
          02 out-assoc                 pic 9(04) value 0.
          02 out-afil                  pic 9(04) value 0.
          02 out-total                 pic 9(04) value 0.
          02 tot-assoc                 pic 9(04) value 0.
          02 tot-afil                  pic 9(04) value 0.
          02 tot-total                 pic 9(04) value 0.
      *
       01 cab-abav.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(01) value x"0e".
          02 filler                    pic x(08) value "ABAV/SP ".
          02 filler                    pic x(01) value x"14".
          02 filler                    pic x(01) value x"0f".
          02 filler                    pic x(58) value
          "Associcao Brasileira de Agencias de Viagens de Sao Paulo".
          02 filler                    pic x(01) value x"12".
      *
       01 cab-prog.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(18) value
          "Quadro Estatistico".
          02 filler                    pic x(46) value spaces.
          02 cab-data                  pic x(08) value spaces.
      *
       01 cab-01.
          02 filler                    pic x(21) value spaces.
          02 filler                    pic x(01) value "+".
          02 filler                    pic x(13) value all "-".
          02 filler                    pic x(01) value "+".
          02 filler                    pic x(06) value all "-".
          02 filler                    pic x(01) value "+".
          02 filler                    pic x(06) value all "-".
          02 filler                    pic x(01) value "+".
          02 filler                    pic x(06) value all "-".
          02 filler                    pic x(01) value "+".
      *
       01 cab-02.
          02 filler                    pic x(21) value spaces.
          02 filler                    pic x(01) value "|".
          02 det-01                    pic x(13) value spaces.
          02 filler                    pic x(01) value "|".
          02 filler                    pic x(01) value spaces.
          02 det-02                    pic x(04) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(01) value "|".
          02 filler                    pic x(01) value spaces.
          02 det-03                    pic x(04) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(01) value "|".
          02 filler                    pic x(01) value spaces.
          02 det-04                    pic x(04) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(01) value "|".
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
          02 line 10 column 05 foreground-color 06 background-color 01 
             highlight value "ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍ¸".
          02 line 11 column 05 foreground-color 06 background-color 01 
             highlight value "³             ³ Assoc ³ Afil  ³ Total ³".
          02 line 12 column 05 foreground-color 06 background-color 01 
             highlight value "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄ´".
          02 line 13 column 05 foreground-color 06 background-color 01 
             highlight value "³  Capital    ³       ³       ³       ³".
          02 line 14 column 05 foreground-color 06 background-color 01 
             highlight value "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄ´".
          02 line 15 column 05 foreground-color 06 background-color 01 
             highlight value "³  Interior   ³       ³       ³       ³".
          02 line 16 column 05 foreground-color 06 background-color 01 
             highlight value "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄ´".
          02 line 17 column 05 foreground-color 06 background-color 01 
             highlight value "³  Outras Loc.³       ³       ³       ³".
          02 line 18 column 05 foreground-color 06 background-color 01 
             highlight value "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄ´".
          02 line 19 column 05 foreground-color 06 background-color 01 
             highlight value "³  Totais     ³       ³       ³       ³".
          02 line 20 column 05 foreground-color 06 background-color 01 
             highlight value "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄ´".
      *
       01 tela-02.
          02 line 21 column 06 foreground-color 02 background-color 03
             pic x(37) from spaces.
          02 line 21 column 06 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 21 column 13 foreground-color 02 background-color 03
             highlight value "I".
          02 line 21 column 14 foreground-color 05 background-color 03
             value ") interromper   (".
          02 line 21 column 31 foreground-color 02 background-color 03
             highlight value "P".
          02 line 21 column 32 foreground-color 05 background-color 03
             value ") imprimir".
      *
       01 tela-03.
          02 line 21 column 06 foreground-color 02 background-color 03
             pic x(37) from spaces.
          02 line 21 column 06 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 21 column 13 foreground-color 02 background-color 03
             highlight value "C".
          02 line 21 column 14 foreground-color 05 background-color 03
             value ")ontinuar   (".
          02 line 21 column 27 foreground-color 02 background-color 03
             highlight value "F".
          02 line 21 column 28 foreground-color 05 background-color 03
             value ")inalizar".
      *
       01 tela-mensagem-cad.
          02 line 21 column 06 foreground-color 07 background-color 01
             highlight pic x(37) from mensagem.
      *
       01 tela-erro-cad.
          02 line 21 column 06 beep reverse-video pic x(37) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 21 column 06 foreground-color 04 background-color 01
             pic x(37) from spaces.
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
           move 04 to box-col.
           move 09 to box-lin.
           move 42 to box-col-f.
           move 21 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 14 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-01. 
           perform dsp-cap-assoc thru dsp-tot-total.
      *
       lab-01.
           display tela-limpa-cad.
           perform sec-estatistica.
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

      *
       rot-le-proximo.
           move 0 to erro.
           read arqab01 next at end move 1 to erro.
           if ab01-status = "9D"
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
       rot-interrompe.
           call "C_Readkey".
           move return-code to campo-kbd.
           if kbd2 = 73 or 105 or 27
              display tela-03
              move 0 to campo-kbd
              perform until kbd2 = 67 or 99 or 70 or 102 or 27
                      perform rot-keypress
              end-perform
              if kbd2 = 70 or 102 or 27
                 move "F" to resposta
              else
                 display tela-02
              end-if
           else
              if flag-fim = "S"
                 if kbd2 = 80 or 112
                    display tela-limpa-cad
                    perform sec-imprime
                    display tela-02
                 end-if
              end-if
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
           accept resposta at 1868 with auto foreground-color 01
                                             background-color 01.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
      *  Sequencia para dar display
      *
       dsp-cap-assoc.
           display cap-assoc at 1321 with foreground-color 15 
                   background-color 01.
      *
       dsp-cap-afil.
           display cap-afil at 1329 with foreground-color 15 
                   background-color 01.
      *
       dsp-cap-total.
           display cap-total at 1337 with foreground-color 15 
                   background-color 01.
      *
       dsp-int-assoc.
           display int-assoc at 1521 with foreground-color 15 
                   background-color 01.
      *
       dsp-int-afil.
           display int-afil at 1529 with foreground-color 15 
                   background-color 01.
      *
       dsp-int-total.
           display int-total at 1537 with foreground-color 15 
                   background-color 01.
      *
       dsp-out-assoc.
           display out-assoc at 1721 with foreground-color 15 
                   background-color 01.
      *
       dsp-out-afil.
           display out-afil at 1729 with foreground-color 15 
                   background-color 01.
      *
       dsp-out-total.
           display out-total at 1737 with foreground-color 15 
                   background-color 01.
      *
       dsp-tot-assoc.
           display tot-assoc at 1921 with foreground-color 15 
                   background-color 01.
      *
       dsp-tot-afil.
           display tot-afil at 1929 with foreground-color 15 
                   background-color 01.
      *
       dsp-tot-total.
           display tot-total at 1937 with foreground-color 15 
                   background-color 01.
      *
       display-erro-usr.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
       sec-estatistica section.
      *
       lab-est-00.
           perform rot-open-ab01.
           if erro not = 0
              go to lab-est-fim
           end-if.
           display tela-limpa-cad.
           if param-prioridade < 1
              perform display-erro-usr
              go to lab-est-fim
           end-if.
           move low-values to ab01-chave.
           start arqab01 key is not less ab01-chave invalid key
                 perform err-leitura-ab01
                 go to lab-est-fim
           end-start.
           display tela-02.
      *
       lab-est-01.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-est-fim
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              go to lab-est-fim
           end-if.           
           if ab01-chave = high-values
              move chave-ant to ab01-chave
              start arqab01 key is greater ab01-chave invalid key
                    perform err-leitura-ab01
                    go to lab-est-fim
              end-start
              move "S" to flag-fim
              call "C_Wait" using by value campo-dorme
              go to lab-est-01
           end-if.
           if ab01-situacao not = 1 and 5
              go to lab-est-01
           end-if.
           move ab01-chave to chave-ant.
           if ab01-uf = "SP"
              if ab01-cep < 05901000
                 evaluate true
                          when ab01-condicao = "A"
                               add 1 to cap-assoc cap-total
                                        tot-assoc tot-total
                               perform dsp-cap-assoc
                               perform dsp-cap-total
                               perform dsp-tot-assoc
                               perform dsp-tot-total
                          when ab01-condicao = "F"
                               add 1 to cap-afil cap-total
                                        tot-afil tot-total
                               perform dsp-cap-afil
                               perform dsp-cap-total
                               perform dsp-tot-afil
                               perform dsp-tot-total
                 end-evaluate
              else
                 evaluate true
                          when ab01-condicao = "A"
                               add 1 to int-assoc int-total 
                                        tot-assoc tot-total
                               perform dsp-int-assoc
                               perform dsp-int-total
                               perform dsp-tot-assoc
                               perform dsp-tot-total
                          when ab01-condicao = "F"
                               add 1 to int-afil int-total 
                                        tot-afil tot-total
                               perform dsp-int-afil
                               perform dsp-int-total
                               perform dsp-tot-afil
                               perform dsp-tot-total
                 end-evaluate
              end-if
           else
              evaluate true
                       when ab01-condicao = "A"
                            add 1 to out-assoc out-total 
                                     tot-assoc tot-total
                            perform dsp-out-assoc
                            perform dsp-out-total
                            perform dsp-tot-assoc
                            perform dsp-tot-total
                       when ab01-condicao = "F"
                            add 1 to out-afil out-total 
                                     tot-afil tot-total
                            perform dsp-out-afil
                            perform dsp-out-total
                            perform dsp-tot-afil
                            perform dsp-tot-total
              end-evaluate
           end-if.
           go to lab-est-01.
      *
       lab-est-fim.
           perform rot-close-ab01.
           exit.
      *
       sec-imprime section.
      *
       lab-imp-00.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-fim
           end-if.
      *
       lab-imp-01.
           write reg-imp from cab-abav after 1 line.
           move param-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           write reg-imp from cab-prog after 2 lines.
           write reg-imp from tracos after 1 line.
           write reg-imp from cab-01 after 5 lines.
           move "Assoc" to det-02.
           move "Afil" to det-03.
           move "Total" to det-04.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-01 after 1 line.
           move "  Capital" to det-01.
           move cap-assoc to det-02.
           move cap-afil to det-03.
           move cap-total to det-04.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-01 after 1 lines.
           move "  Interior" to det-01.
           move int-assoc to det-02.
           move int-afil to det-03.
           move int-total to det-04.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-01 after 1 lines.
           move "  Outras Loc." to det-01.
           move out-assoc to det-02.
           move out-afil to det-03.
           move out-total to det-04.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-01 after 1 lines.
           move "  Totais" to det-01.
           move tot-assoc to det-02.
           move tot-afil to det-03.
           move tot-total to det-04.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-01 after 1 line.
           write reg-imp from spaces before page.
      *
       lab-imp-fim.
           perform rot-close-imp.
           exit.