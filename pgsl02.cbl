      ***************************************************************
      *                                                             *
      *  A B A V / S P -  I N F O R M A T I C A   :::  PGSL02       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Quadro de Inscricoes :                                     *
      *                                                             *
      *  Data da ultima alteracao:    05/04/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgsl02.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqsl01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is sl01-chave
                  alternate record key is sl01-chave-1 with duplicates
                  alternate record key is sl01-chave-2 with duplicates
                  alternate record key is sl01-chave-3 with duplicates
                  alternate record key is sl01-chave-4 with duplicates
                  file status is sl01-status.
      *
           select arqimp assign to printer
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
       data division.
       file section.
      *    
       copy fdsl01.lib.
      *
       fd arqimp

       label record is standard
       value of file-id is impress
       data record is reg-imp.

       01 reg-imp                      pic x(300).
      *
       working-storage section.
      *
       01 sl01-status                  pic x(02) value "00".
       01 sl01-stat                    pic x(01) value "F".
      *
       01 nome-arq-sl01.
          02 sl01-dir                  pic x(03) value "SL1".
          02 filler                    pic x(01) value "\".
          02 sl01-nome                 pic x(08) value "ARQSL01A".
          02 filler                    pic x(01) value ".".
          02 sl01-ext                  pic x(03) value "DAT".
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGSL02".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(45) value spaces.
       01 limpa-aux                    pic x(15) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 flag-fim                     pic x(01) value "N".
       01 campo-dorme                  pic 9(04) comp-5 value 3.
       01 chave-ant                    pic 9(05) value 0.
       01 tracos                       pic x(78) value all "-".
      *
       01 campos.
          02 cap-agen                  pic 9(04) value 0.
          02 cap-vist                  pic 9(04) value 0.
          02 cap-impr                  pic 9(04) value 0.
          02 cap-org                   pic 9(04) value 0.
          02 cap-prof                  pic 9(04) value 0.
          02 cap-prom                  pic 9(04) value 0.
          02 cap-total                 pic 9(04) value 0.
          02 int-agen                  pic 9(04) value 0.
          02 int-vist                  pic 9(04) value 0.
          02 int-impr                  pic 9(04) value 0.
          02 int-org                   pic 9(04) value 0.
          02 int-prof                  pic 9(04) value 0.
          02 int-prom                  pic 9(04) value 0.
          02 int-total                 pic 9(04) value 0.
          02 out-agen                  pic 9(04) value 0.
          02 out-vist                  pic 9(04) value 0.
          02 out-impr                  pic 9(04) value 0.
          02 out-org                   pic 9(04) value 0.
          02 out-prof                  pic 9(04) value 0.
          02 out-prom                  pic 9(04) value 0.
          02 out-total                 pic 9(04) value 0.
          02 tot-agen                  pic 9(04) value 0.
          02 tot-vist                  pic 9(04) value 0.
          02 tot-impr                  pic 9(04) value 0.
          02 tot-org                   pic 9(04) value 0.
          02 tot-prof                  pic 9(04) value 0.
          02 tot-prom                  pic 9(04) value 0.
          02 tot-total                 pic 9(04) value 0.
      *
       01 cab-abav.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(01) value x"0e".
          02 filler                    pic x(08) value "ABAV/SP ".
          02 filler                    pic x(01) value x"14".
          02 filler                    pic x(01) value x"0f".
          02 filler                    pic x(65) value
          "Associcao Brasileira de Agencias de Viagens de Sao Paulo - S.
      -   "P.T.".
          02 filler                    pic x(01) value x"12".
      *
       01 cab-prog.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(20) value
          "Quadro de Inscricoes".
          02 filler                    pic x(46) value spaces.
          02 cab-data                  pic x(08) value spaces.
      *
       01 cab-01.
          02 filler                    pic x(06) value spaces.
          02 filler                    pic x(01) value "+".
          02 filler                    pic x(13) value all "-".
          02 filler                    pic x(01) value "+".
          02 filler                    pic x(06) value all "-".
          02 filler                    pic x(01) value "+".
          02 filler                    pic x(06) value all "-".
          02 filler                    pic x(01) value "+".
          02 filler                    pic x(06) value all "-".
          02 filler                    pic x(01) value "+".
          02 filler                    pic x(06) value all "-".
          02 filler                    pic x(01) value "+".
          02 filler                    pic x(06) value all "-".
          02 filler                    pic x(01) value "+".
          02 filler                    pic x(06) value all "-".
          02 filler                    pic x(01) value "+".
          02 filler                    pic x(07) value all "-".
          02 filler                    pic x(01) value "+".
      *
       01 cab-02.
          02 filler                    pic x(06) value spaces.
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
          02 filler                    pic x(01) value spaces.
          02 det-05                    pic x(04) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(01) value "|".
          02 filler                    pic x(01) value spaces.
          02 det-06                    pic x(04) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(01) value "|".
          02 filler                    pic x(01) value spaces.
          02 det-07                    pic x(04) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(01) value "|".
          02 filler                    pic x(01) value spaces.
          02 det-08                    pic x(05) value spaces.
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
          02 line 08 column 05 foreground-color 06 background-color 01 
             highlight value 
             "ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍ
      -      "ÍÍÍÍÍÍ¸".
          02 line 09 column 05 foreground-color 06 background-color 01 
             highlight value 
             "³             ³ Agen ³ Vist ³ Impr ³ Org  ³ Prof ³ Prom ³ 
      -      "Total ³".
          02 line 10 column 05 foreground-color 06 background-color 01 
             highlight value 
             "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄ
      -      "ÄÄÄÄÄÄ´".
          02 line 11 column 05 foreground-color 06 background-color 01 
             highlight value 
             "³  Capital    ³      ³      ³      ³      ³      ³      ³ 
      -      "      ³".
          02 line 12 column 05 foreground-color 06 background-color 01 
             highlight value 
             "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄ
      -      "ÄÄÄÄÄÄ´".
          02 line 13 column 05 foreground-color 06 background-color 01 
             highlight value 
             "³  Interior   ³      ³      ³      ³      ³      ³      ³  
      -      "      ³".
          02 line 14 column 05 foreground-color 06 background-color 01 
             highlight value 
             "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄ
      -      "ÄÄÄÄÄÄ´".
          02 line 15 column 05 foreground-color 06 background-color 01 
             highlight value 
             "³  Outras Loc.³      ³      ³      ³      ³      ³      ³  
      -      "      ³".
          02 line 16 column 05 foreground-color 06 background-color 01 
             highlight value 
             "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄ
      -      "ÄÄÄÄÄÄ´".
          02 line 17 column 05 foreground-color 06 background-color 01 
             highlight value 
             "³  Totais     ³      ³      ³      ³      ³      ³      ³ 
      -      "      ³".
          02 line 18 column 05 foreground-color 06 background-color 01 
             highlight value 
             "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄ
      -      "ÄÄÄÄÄÄ´".
      *
       01 tela-02.
          02 line 19 column 06 foreground-color 05 background-color 03
             pic x(63) from spaces.
          02 line 19 column 06 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 19 column 13 foreground-color 02 background-color 03
             highlight value "I".
          02 line 19 column 14 foreground-color 05 background-color 03
             value ") para interromper   (".
          02 line 19 column 36 foreground-color 02 background-color 03
             highlight value "P".
          02 line 19 column 37 foreground-color 05 background-color 03
             value ") para imprimir".
      *
       01 tela-03.
          02 line 19 column 06 foreground-color 05 background-color 03
             pic x(63) from spaces.
          02 line 19 column 06 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 19 column 13 foreground-color 02 background-color 03
             highlight value "C".
          02 line 19 column 14 foreground-color 05 background-color 03
             value ")ontinuar   (".
          02 line 19 column 27 foreground-color 02 background-color 03
             highlight value "F".
          02 line 19 column 28 foreground-color 05 background-color 03
             value ")inalizar".
      *
      *
       01 tela-mensagem-cad.
          02 line 19 column 06 foreground-color 07 background-color 01
             highlight pic x(63) from mensagem.
      *
       01 tela-erro-cad.
          02 line 19 column 06 beep reverse-video pic x(63) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 19 column 06 foreground-color 01 background-color 01
             pic x(63) from spaces.
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
           move 07 to box-lin.
           move 68 to box-col-f.
           move 19 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 14 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-01. 
           perform dsp-cap-agen thru dsp-tot-total.
      *
       lab-01.
           display tela-limpa-cad.
           perform sec-inscricao.
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
           read arqsl01 next at end move 1 to erro.
           if sl01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-sl01.
           move 0 to erro.
           if sl01-stat = "F"
              open i-o arqsl01
              if sl01-status not = "00"
                 move 
                 " Erro de abertura no ARQSL01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to sl01-stat
               end-if
           end-if.
      *
       rot-close-sl01.
           if sl01-stat = "A"
              close arqsl01
              move "F" to sl01-stat
           end-if.
      *
       err-leitura-sl01.
           move " Erro de leitura - ARQSL01A.DAT - Tecle <Enter>" to
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
       dsp-cap-agen.
           display cap-agen at 1121 with foreground-color 15 
                   background-color 01.
      *
       dsp-cap-vist.
           display cap-vist at 1128 with foreground-color 15 
                   background-color 01.
      *
       dsp-cap-impr.
           display cap-impr at 1135 with foreground-color 15 
                   background-color 01.
      *
       dsp-cap-org.
           display cap-org at 1142 with foreground-color 15 
                   background-color 01.
      *
       dsp-cap-prof.
           display cap-prof at 1149 with foreground-color 15 
                   background-color 01.
      *
       dsp-cap-prom.
           display cap-prom at 1156 with foreground-color 15 
                   background-color 01.
      *
       dsp-cap-total.
           display cap-total at 1164 with foreground-color 15 
                   background-color 01.
      *
       dsp-int-agen.
           display int-agen at 1321 with foreground-color 15 
                   background-color 01.
      *
       dsp-int-vist.
           display int-vist at 1328 with foreground-color 15 
                   background-color 01.
      *
       dsp-int-impr.
           display int-impr at 1335 with foreground-color 15 
                   background-color 01.
      *
       dsp-int-org.
           display int-org at 1342 with foreground-color 15 
                   background-color 01.
      *
       dsp-int-prof.
           display int-prof at 1349 with foreground-color 15 
                   background-color 01.
      *
       dsp-int-prom.
           display int-prom at 1356 with foreground-color 15 
                   background-color 01.
      *
       dsp-int-total.
           display int-total at 1364 with foreground-color 15 
                   background-color 01.
      *
       dsp-out-agen.
           display out-agen at 1521 with foreground-color 15 
                   background-color 01.
      *
       dsp-out-vist.
           display out-vist at 1528 with foreground-color 15 
                   background-color 01.
      *
       dsp-out-impr.
           display out-impr at 1535 with foreground-color 15 
                   background-color 01.
      *
       dsp-out-org.
           display out-org at 1542 with foreground-color 15 
                   background-color 01.
      *
       dsp-out-prof.
           display out-prof at 1549 with foreground-color 15 
                   background-color 01.
      *
       dsp-out-prom.
           display out-prom at 1556 with foreground-color 15 
                   background-color 01.
      *
       dsp-out-total.
           display out-total at 1564 with foreground-color 15 
                   background-color 01.
      *
       dsp-tot-agen.
           display tot-agen at 1721 with foreground-color 15 
                   background-color 01.
      *
       dsp-tot-vist.
           display tot-vist at 1728 with foreground-color 15 
                   background-color 01.
      *
       dsp-tot-impr.
           display tot-impr at 1735 with foreground-color 15 
                   background-color 01.
      *
       dsp-tot-org.
           display tot-org at 1742 with foreground-color 15 
                   background-color 01.
      *
       dsp-tot-prof.
           display tot-prof at 1749 with foreground-color 15 
                   background-color 01.
      *
       dsp-tot-prom.
           display tot-prom at 1756 with foreground-color 15 
                   background-color 01.
      *
       dsp-tot-total.
           display tot-total at 1764 with foreground-color 15 
                   background-color 01.
      *
       display-erro-usr.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
       sec-inscricao section.
      *
       lab-ins-00.
           perform rot-open-sl01.
           if erro not = 0
              go to lab-ins-fim
           end-if.
           display tela-limpa-cad.
           if param-prioridade < 1
              perform display-erro-usr
              go to lab-ins-fim
           end-if.
           move low-values to sl01-chave.
           start arqsl01 key is not less sl01-chave invalid key
                 perform err-leitura-sl01
                 go to lab-ins-fim
           end-start.
           display tela-02.
      *
       lab-ins-01.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-ins-fim
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              go to lab-ins-fim
           end-if.           
           if sl01-chave = high-values
              move chave-ant to sl01-chave
              start arqsl01 key is greater sl01-chave invalid key
                    perform err-leitura-sl01
                    go to lab-ins-fim
              end-start
              move "S" to flag-fim
              call "C_Wait" using by value campo-dorme
              go to lab-ins-01
           end-if.
           move sl01-chave to chave-ant.
           if sl01-uf = "SP"
              if sl01-cidade = "SAO PAULO"
                 evaluate true
                          when sl01-grupo = "A"
                               add 1 to cap-agen cap-total 
                                        tot-agen tot-total
                               perform dsp-cap-agen
                               perform dsp-cap-total
                               perform dsp-tot-agen
                               perform dsp-tot-total
                          when sl01-grupo = "V"
                               add 1 to cap-vist cap-total 
                                        tot-vist tot-total
                               perform dsp-cap-vist
                               perform dsp-cap-total
                               perform dsp-tot-vist
                               perform dsp-tot-total
                          when sl01-grupo = "I"
                               add 1 to cap-impr cap-total 
                                        tot-impr tot-total
                               perform dsp-cap-impr
                               perform dsp-cap-total
                               perform dsp-tot-impr
                               perform dsp-tot-total
                          when sl01-grupo = "O"
                               add 1 to cap-org cap-total 
                                        tot-org tot-total
                               perform dsp-cap-org
                               perform dsp-cap-total
                               perform dsp-tot-org
                               perform dsp-tot-total
                          when sl01-grupo = "F"
                               add 1 to cap-prof cap-total 
                                        tot-prof tot-total
                               perform dsp-cap-prof
                               perform dsp-cap-total
                               perform dsp-tot-prof
                               perform dsp-tot-total
                          when sl01-grupo = "M"
                               add 1 to cap-prom cap-total 
                                        tot-prom tot-total
                               perform dsp-cap-prom
                               perform dsp-cap-total
                               perform dsp-tot-prom
                               perform dsp-tot-total
                 end-evaluate
              else
                 evaluate true
                          when sl01-grupo = "A"
                               add 1 to int-agen int-total 
                                        tot-agen tot-total
                               perform dsp-int-agen
                               perform dsp-int-total
                               perform dsp-tot-agen
                               perform dsp-tot-total
                          when sl01-grupo = "V"
                               add 1 to int-vist int-total 
                                        tot-vist tot-total
                               perform dsp-int-vist
                               perform dsp-int-total
                               perform dsp-tot-vist
                               perform dsp-tot-total
                          when sl01-grupo = "I"
                               add 1 to int-impr int-total 
                                        tot-impr tot-total
                               perform dsp-int-impr
                               perform dsp-int-total
                               perform dsp-tot-impr
                               perform dsp-tot-total
                          when sl01-grupo = "O"
                               add 1 to int-org int-total 
                                        tot-org tot-total
                               perform dsp-int-org
                               perform dsp-int-total
                               perform dsp-tot-org
                               perform dsp-tot-total
                          when sl01-grupo = "F"
                               add 1 to int-prof int-total 
                                        tot-prof tot-total
                               perform dsp-int-prof
                               perform dsp-int-total
                               perform dsp-tot-prof
                               perform dsp-tot-total
                          when sl01-grupo = "M"
                               add 1 to int-prom int-total 
                                        tot-prom tot-total
                               perform dsp-int-prom
                               perform dsp-int-total
                               perform dsp-tot-prom
                               perform dsp-tot-total
                 end-evaluate
              end-if
           else
              evaluate true
                       when sl01-grupo = "A"
                            add 1 to out-agen out-total 
                                     tot-agen tot-total
                            perform dsp-out-agen
                            perform dsp-out-total
                            perform dsp-tot-agen
                            perform dsp-tot-total
                       when sl01-grupo = "V"
                            add 1 to out-vist out-total 
                                     tot-vist tot-total
                            perform dsp-out-vist
                            perform dsp-out-total
                            perform dsp-tot-vist
                            perform dsp-tot-total
                       when sl01-grupo = "I"
                            add 1 to out-impr out-total 
                                     tot-impr tot-total
                            perform dsp-out-impr
                            perform dsp-out-total
                            perform dsp-tot-impr
                            perform dsp-tot-total
                       when sl01-grupo = "O"
                            add 1 to out-org out-total 
                                     tot-org tot-total
                            perform dsp-out-org
                            perform dsp-out-total
                            perform dsp-tot-org
                            perform dsp-tot-total
                       when sl01-grupo = "F"
                            add 1 to out-prof out-total 
                                     tot-prof tot-total
                            perform dsp-out-prof
                            perform dsp-out-total
                            perform dsp-tot-prof
                            perform dsp-tot-total
                       when sl01-grupo = "M"
                            add 1 to out-prom out-total 
                                     tot-prom tot-total
                            perform dsp-out-prom
                            perform dsp-out-total
                            perform dsp-tot-prom
                            perform dsp-tot-total
              end-evaluate
           end-if.
           go to lab-ins-01.
      *
       lab-ins-fim.
           perform rot-close-sl01.
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
           move "Agen" to det-02.
           move "Vist" to det-03.
           move "Impr" to det-04.
           move "Org " to det-05.
           move "Prof" to det-06.
           move "Prom" to det-07.
           move "Total" to det-08.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-01 after 1 line.
           move "  Capital" to det-01.
           move cap-agen to det-02.
           move cap-vist to det-03.
           move cap-impr to det-04.
           move cap-org to det-05.
           move cap-prof to det-06.
           move cap-prom to det-07.
           move cap-total to det-08.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-01 after 1 lines.
           move "  Interior" to det-01.
           move int-agen to det-02.
           move int-vist to det-03.
           move int-impr to det-04.
           move int-org to det-05.
           move int-prof to det-06.
           move int-prom to det-07.
           move int-total to det-08.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-01 after 1 lines.
           move "  Outras Loc." to det-01.
           move out-agen to det-02.
           move out-vist to det-03.
           move out-impr to det-04.
           move out-org to det-05.
           move out-prof to det-06.
           move out-prom to det-07.
           move out-total to det-08.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-01 after 1 lines.
           move "  Totais" to det-01.
           move tot-agen to det-02.
           move tot-vist to det-03.
           move tot-impr to det-04.
           move tot-org to det-05.
           move tot-prof to det-06.
           move tot-prom to det-07.
           move tot-total to det-08.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-01 after 1 line.
           write reg-imp from spaces before page.
      *
       lab-imp-fim.
           perform rot-close-imp.
           exit.