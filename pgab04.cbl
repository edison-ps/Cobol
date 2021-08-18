      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGAB03       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao do cadastro TKTS:                               *
      *                                                             *
      *  Data da ultima alteracao:    05/12/93     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgab04.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqab03 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab03-chave
                  alternate record key is ab03-chave-1
                  file status is ab02-status.
      *
           select arqab02 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab02-chave
                  alternate record key is ab02-chave-1
                  alternate record key is ab02-chave-2 with duplicates
                  alternate record key is ab02-chave-3 with duplicates
                  file status is ab02-status.
      *
       data division.
       file section.
      *    
       copy fdab02.lib.
      *    
       copy fdab03.lib.
      *
       working-storage section.
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
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGAB04".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(45) value spaces.
       01 limpa-aux                    pic x(10) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 codigo                    pic 9(05) value 0.
          02 descricao                 pic x(40) value spaces.
          02 descricao-a               pic x(40) value spaces.
          02 data-i                    pic 9(06) value 0.
          02 data-disp-a               pic x(08) value spaces.
          02 tkt                       pic 9(03) value 0.
          02 aerop                     pic 9(01) value 0.
      * 
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       01 cab-usr.
          02 filler                    pic x(10) value "Usuario.:".
          02 cab-usuario               pic x(10) value spaces.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Data....:".
          02 cab-data                  pic x(08) value spaces.
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
          02 line 15 column 06 foreground-color 02 background-color 06
             value "Codigo........:".
          02 line 16 column 06 foreground-color 02 background-color 06
             value "Consorciado...:".
          02 line 17 column 06 foreground-color 02 background-color 06
             value "Aeroporto.....:".
          02 line 18 column 06 foreground-color 02 background-color 06
             value "Data..........:".
          02 line 18 column 46 foreground-color 02 background-color 06
             value "Qtde. TKTS....:".
      *
       01 tela-02.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight value "F1".
          02 line 20 column 07 foreground-color 01 background-color 02
             value "-Help".
          02 line 20 column 15 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 20 column 17 foreground-color 01 background-color 02
             value "-Consultas".
      *
       01 tela-03.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 07 background-color 
             02 highlight value "F1".
          02 line 20 column 08 foreground-color 01 background-color 02
             value "-Help".
          02 line 20 column 16 foreground-color 07 background-color 02
             highlight value "C".
          02 line 20 column 17 foreground-color 01 background-color 02
             value "odigo".
          02 line 20 column 27 foreground-color 07 background-color 02
             highlight value "D".
          02 line 20 column 28 foreground-color 01 background-color 02
             value "escricao".
      *
       01 tela-04.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 07 background-color 02 
             highlight value "F2".
          02 line 20 column 08 foreground-color 01 background-color 02
             value "-Alt".
          02 line 20 column 15 foreground-color 07 background-color 02 
             highlight value "F3".
          02 line 20 column 17 foreground-color 01 background-color 02
             value "-Exc".
          02 line 20 column 25 foreground-color 07 background-color 02
             highlight value "Home".
          02 line 20 column 29 foreground-color 01 background-color 02
             value "-Inic".
          02 line 20 column 38 foreground-color 07 background-color 02
             highlight value "End".
          02 line 20 column 41 foreground-color 01 background-color 02
             value "-Fim".
          02 line 20 column 49 foreground-color 07 background-color 02
             highlight value "PgDown".
          02 line 20 column 54 foreground-color 01 background-color 02
             value "-Prox".
          02 line 20 column 62 foreground-color 07 background-color 02
             highlight value "PgUp".
          02 line 20 column 66 foreground-color 01 background-color 02
             value "-Ant".
      *
       01 tela-05.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 07 foreground-color 07 background-color 02
             highlight value "1".
          02 line 20 column 08 foreground-color 01 background-color 02
             value "-Congonhas (CHG)".
          02 line 20 column 27 foreground-color 07 background-color 02
             highlight value "2".
          02 line 20 column 28 foreground-color 01 background-color 02
             value "-Guarulhos (GRU)".
      *
       01 tela-06.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight value "Registro gravado - Tecle <Enter>".
      *
       01 tela-09.
          02 line 14 column 65 foreground-color 07 background-color 06
             highlight value "Inclusao".
      *
       01 tela-10.
          02 line 14 column 65 foreground-color 07 background-color 06
             highlight value "Consulta".
      *
       01 tela-mensagem-cad.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 20 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 20 column 05 foreground-color 06 background-color 06
             pic x(68) from spaces.
      *
       01 tela-mensagem-cad-tkt.
          02 line 18 column 17 foreground-color 07 background-color 02
             highlight pic x(40) from mensagem.
      *
       01 tela-erro-cad-tkt.
          02 line 18 column 17 beep reverse-video pic x(40) from 
             mensagem.
      *
       01 tela-limpa-cad-tkt.
          02 line 18 column 17 foreground-color 06 background-color 06
             pic x(40) from spaces.
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
           move 12 to box-lin.
           move 72 to box-col-f.
           move 20 to box-lin-f.
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
           perform sec-inclusao.
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
       rot-move-ab03.
           move codigo to ab03-codigo.
           move data-i to ab03-data-i in ab03-chave
                          ab03-data-i in ab03-chave-1.
           move descricao to ab03-descricao.
           move descricao-a to ab03-descricao-a.
           move tkt to ab03-tkt. 
           move aerop to ab03-aerop in ab03-chave
                         ab03-aerop in ab03-chave-1.
           move param-usr to ab03-usuario.
           move param-data to ab03-data.
      *
       rot-move-campos.
           move ab03-codigo to codigo.
           move ab03-descricao-a to descricao-a.
           move ab03-descricao to descricao.
           move ab03-data-i in ab03-chave to dias-corr data-i.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to data-disp-a.
           move ab03-tkt to tkt.
           move ab03-aerop in ab03-chave to aerop.
           move ab03-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move ab03-usuario to cab-usuario.
           move ab03-tkt to tkt.
      *
       rot-le-ab02.
           move 0 to erro.
           read arqab02 invalid key move 1 to erro.
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab02.
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
       rot-ponteiro.
           move 0 to erro.
           start arqab03 key is equal ab03-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-ab02
           end-start.
      *
       rot-le-ab03-lock.
           move 0 to erro.
           read arqab03 next. 
           if ab03-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ab03-lock
           end-if.
           read arqab03 with kept lock.
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
       rot-erro-leitura-ab02.
           move " Erro de leitura - ARQAB02A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-open-ab03.
           move 0 to erro.
           if ab03-stat = "F"
              open i-o arqab03
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
       rot-inic-arquivo.
           perform lmp-codigo thru lmp-tkt.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-codigo thru lmp-tkt.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-display.
           perform rot-move-campos.
           perform dsp-codigo thru dsp-tkt.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
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
           accept resposta at 1968 with auto foreground-color 06
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
       acc-codigo.
           accept codigo at 1522 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-descricao.
           accept descricao at 1622 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-aerop.
           accept aerop at 1722 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-data-i.
           accept data-i at 1822 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
       acc-tkt.
           accept tkt at 1862 with auto update prompt
                  foreground-color 15 background-color 06.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-codigo.
           display codigo at 1522 with foreground-color 15 
                   background-color 06.
      *
       dsp-descricao.
           display descricao-a at 1622 with foreground-color 15 
                   background-color 06.
      *
       dsp-aerop.
           display aerop at 1722 with foreground-color 15 
                   background-color 06.
      *
       dsp-data-i.
           display data-disp-a at 1822 with foreground-color 15 
                   background-color 06.
      *
       dsp-tkt.
           display tkt at 1862 with foreground-color 15 
                   background-color 06.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-codigo.
           display limpa at 1522 with foreground-color 15 
                   background-color 06.
      *
       lmp-descricao.
           display limpa at 1622 with foreground-color 15 
                   background-color 06.
      *
       lmp-aerop.
           display limpa-aux at 1722 with foreground-color 15 
                   background-color 06.
      *
       lmp-data-i.
           display limpa-aux at 1822 with foreground-color 15 
                   background-color 06.
      *
       lmp-tkt.
           display limpa-aux at 1862 with foreground-color 15 
                   background-color 06.
      *
       sec-inclusao section.
       lab-inc-00-0.
           display tela-limpa-cad.
           perform rot-open-ab02.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-ab03.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           if erro not = 0
              go to lab-inc-fim
           end-if.
      *
       lab-inc-00.
           if param-prioridade < 1
              perform sec-consulta
              go to lab-inc-fim
           end-if.
           display tela-09.
      *
       lab-inc-01.
           display tela-02.
           move 0 to codigo.
           perform lmp-codigo thru lmp-descricao.
           perform acc-codigo.
           if escape-key = 1
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform lmp-codigo
              perform sec-consulta
              display tela-09
              display tela-02
              go to lab-inc-01
           end-if.
           if codigo = 0 
              go to lab-inc-01
           end-if.
           move codigo to ab02-codigo.
           perform rot-le-ab02.
           if erro not = 0
              move " Codigo nao cadastrado - Tecle <Enter>" to 
              mensagem
              display tela-erro-cad
              perform rot-keypress
              display tela-02
              go to lab-inc-01
           end-if.
           move ab02-descricao to descricao.
           move ab02-descricao-a to descricao-a.
           perform dsp-descricao.
           if ab02-flag-cons not = "A" and "S"
              move " Agencia nao consorciada - Tecle <Enter>" to 
              mensagem
              display tela-erro-cad
              perform rot-keypress
              display tela-02
              go to lab-inc-01
           end-if.
           display tela-limpa-cad.
      *
       lab-inc-02.
           display tela-05.
           move 0 to aerop.
           perform lmp-aerop.
           perform acc-aerop.
           if escape-key = 1
              display tela-limpa-cad
              perform lmp-aerop
              go to lab-inc-01
           end-if.
           if aerop not = 1 and 2
              go to lab-inc-02
           end-if.
           display tela-limpa-cad.
           move 0 to data-i.
      *
       lab-inc-03.
           perform lmp-data-i.
           perform acc-data-i.
           if escape-key = 1 
              perform lmp-data-i
              go to lab-inc-02
           end-if.
           if data-i = 0
              go to lab-inc-03
           end-if.
           move data-i to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              move 0 to data-i
              go to lab-inc-03
           end-if.
           move data-disp to data-disp-a.
           perform dsp-data-i.
           move dias-corr to data-i ab03-data-i in ab03-chave.
           move codigo to ab03-codigo.
           move aerop to ab03-aerop in ab03-chave. 
           perform rot-le-ab03.
           if erro = 0
              move " Movimento ja cadastrado - Tecle <Enter>" to 
              mensagem
              display tela-erro-cad
              perform rot-keypress
              display tela-limpa-cad
              move 0 to data-i
              go to lab-inc-03
           end-if.
      *
       lab-inc-04.
           move 0 to tkt.
           perform lmp-tkt.
           perform acc-tkt.
           if escape-key = 1
              perform lmp-tkt
              move data-aux to data-i
              go to lab-inc-03.
           if tkt = 0
              go to lab-inc-04
           end-if.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-inc-05.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-04
           end-if.
           if resposta = "N"
              perform lmp-codigo thru lmp-tkt
              display tela-02
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-05
              end-if
           end-if.
      *
       lab-inc-06.
           perform rot-move-ab03.
           write reg-ab03 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQAB03A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           display tela-06.
           perform rot-keypress.
           display tela-limpa-cad.
           perform lmp-data-i thru lmp-tkt.
           move data-aux to data-i.           
           go to lab-inc-03.
      *
       lab-inc-fim.
           perform rot-close-ab03.
           perform rot-close-ab02.
           exit.
      *
       sec-consulta section.
      *
       lab-cns-00.
           display tela-limpa-cad.
           display tela-10.
      *
       lab-cns-01.
           display tela-03.
           move 0 to kbd2.
           perform until kbd2 = 27
                   perform rot-keypress
                   evaluate true
                            when kbd2 = 67 or 99
                                 display tela-limpa-cad
                                 perform sec-consulta-codigo
                                 display tela-03
                            when kbd2 = 68 or 100
                                 display tela-limpa-cad
                                 perform sec-consulta-descricao
                                 display tela-03
                   end-evaluate
                   display tela-limpa
           end-perform.
      *
       lab-cns-fim.
           display tela-limpa.
           exit.
      *
       sec-consulta-codigo section.
      *
       lab-cns-codigo-00.
           move 0 to codigo.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              perform lmp-codigo
              go to lab-cns-codigo-fim
           end-if.
           if codigo = 0
              perform lmp-codigo
           end-if.
      *
       lab-cns-codigo-00-0.
           move 0 to data-i.
           perform lmp-data-i.
           perform acc-data-i.
           if escape-key = 1 
              perform lmp-data-i
              go to lab-cns-codigo-00
           end-if.
           if data-i = 0
              go to lab-cns-codigo-00-1
           end-if.
           move data-i to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              go to lab-cns-codigo-00-0
           end-if.
           move data-disp to data-disp-a.
           move dias-corr to data-i.
           perform dsp-data-i.
      *
       lab-cns-codigo-00-1.
           move low-values to ab03-chave.
           move codigo to ab03-codigo.
           move data-i to ab03-data-i in ab03-chave.
           display tela-04.
      *
       lab-cns-codigo-00-a.
           start arqab03 key is not less ab03-chave.
           go to lab-cns-codigo-03.
      *
       lab-cns-codigo-01.
           perform rot-le-anterior.
           if erro not = 0 or ab03-codigo = codigo 
              perform rot-inic-arquivo
              start arqab03 key is not less ab03-chave
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if ab03-chave = high-values
              go to lab-cns-codigo-01
           end-if.
           go to lab-cns-codigo-04.
      *
       lab-cns-codigo-02.
           start arqab03 key is less ab03-chave.
      *
       lab-cns-codigo-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ab03
              go to lab-cns-codigo-fim
           end-if.
           if ab03-chave = high-values
              perform rot-fim-arquivo
              start arqab03 key is not less ab03-chave
              move 0 to codigo
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
      *
       lab-cns-codigo-04.
           perform rot-display.
      *
       lab-cns-codigo-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-codigo-03
                    when kbd-aux = 73
                         go to lab-cns-codigo-01
                    when kbd-aux = 71
                         move low-values to ab03-chave
                         go to lab-cns-codigo-00-a
                    when kbd-aux = 79
                         move high-values to ab03-chave
                         go to lab-cns-codigo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-codigo-05
           end-if.
           perform lmp-codigo thru lmp-tkt.
           display tela-limpa-cad.
           go to lab-cns-codigo-00.
      *
       lab-cns-codigo-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-tkt.
           exit.
      *
       sec-consulta-descricao section.
      *
       lab-cns-descricao-00.
           move spaces to descricao.
           perform lmp-descricao.
           perform acc-descricao.
           if escape-key = 1
              perform lmp-descricao
              go to lab-cns-descricao-fim
           end-if.
           move descricao to txt.
           call "C_Text" using by reference campo-txt.
      *
       lab-cns-descricao-00-0.
           move 0 to data-i.
           perform lmp-data-i.
           perform acc-data-i.
           if escape-key = 1 
              perform lmp-data-i
              go to lab-cns-descricao-00
           end-if.
           if data-i = 0
              go to lab-cns-descricao-00-1
           end-if.
           move data-i to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              go to lab-cns-descricao-00-0
           end-if.
           move data-disp to data-disp-a.
           move dias-corr to data-i.
           perform dsp-data-i.
      *
       lab-cns-descricao-00-1.
           move low-values to ab03-chave-1.
           move data-i to ab03-data-i in ab03-chave-1.
           move txt to ab03-descricao.
           display tela-04.
      *
       lab-cns-descricao-00-a.
           start arqab03 key is not less ab03-chave-1.
           go to lab-cns-descricao-03.
      *
       lab-cns-descricao-01.
           perform rot-le-anterior.
           if erro not = 0 or ab03-codigo = codigo
              perform rot-inic-arquivo
              start arqab03 key is not less ab03-chave-1
              move 1 to erro
              go to lab-cns-descricao-05
           end-if.
           if ab03-chave = high-values
              go to lab-cns-descricao-01
           end-if.
           go to lab-cns-descricao-04.
      *
       lab-cns-descricao-02.
           start arqab03 key is less ab03-chave-1.
      *
       lab-cns-descricao-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ab03
              go to lab-cns-descricao-fim
           end-if.
           if ab03-chave = high-values
              perform rot-fim-arquivo
              start arqab03 key is not less ab03-chave-1
              move 0 to codigo
              move 1 to erro
              go to lab-cns-descricao-05
           end-if.
      *
       lab-cns-descricao-04.
           perform rot-display.
      *
       lab-cns-descricao-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-descricao-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-descricao-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-descricao-03
                    when kbd-aux = 73
                         go to lab-cns-descricao-01
                    when kbd-aux = 71
                         move low-values to ab03-chave-1
                         go to lab-cns-descricao-00-a
                    when kbd-aux = 79
                         move high-values to ab03-chave-1
                         go to lab-cns-descricao-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-descricao-05
           end-if.
           perform lmp-codigo thru lmp-tkt.
           display tela-limpa-cad.
           go to lab-cns-descricao-00.
      *
       lab-cns-descricao-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-tkt.
           exit.
      *
       sec-exclusao section.
       lab-exc-00-0.
           display tela-limpa-cad.
           if param-prioridade < 7
              perform display-erro-usr
              go to lab-exc-fim
           end-if.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-exc-fim
           end-if.
      *
       lab-exc-00.
           perform rot-le-ab03-lock.
           perform rot-display
           move "Excluir (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-exc-01.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-exc-fim
           end-if.
           if resposta = "N"
              go to lab-exc-fim
           else
              if resposta not = "S"
                 go to lab-exc-01
              end-if
           end-if.
           delete arqab03 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQAB03A.DAT - Tecle <Enter>
      -           " " to mensagem
                  display tela-erro
                  perform rot-keypress
                  display tela-limpa
                  go to lab-exc-fim
           end-delete.
           move "Registro excluido - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       lab-exc-fim.
           unlock arqab03 record.
           display tela-04.
           exit.
      *
       sec-alteracao section.
       lab-alt-00-0.
           display tela-limpa-cad.
           if param-prioridade < 5
              perform display-erro-usr
              go to lab-alt-fim
           end-if.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-alt-fim
           end-if.
      *
       lab-alt-00.
           perform rot-le-ab03-lock.
           perform rot-display.
      *
       lab-alt-01.
           perform acc-tkt
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           if tkt = 0
              go to lab-alt-01
           end-if.
      *
       lab-alt-02.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-01
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-02
              end-if
           end-if.
           perform rot-move-ab03.
           rewrite reg-ab03 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQAB03A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
                   go to lab-alt-fim
           end-rewrite.
           move "Registro alterado - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       lab-alt-fim.
           unlock arqab03 record.
           display tela-04.
           exit.