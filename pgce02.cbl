      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  PGCE02       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao do cadastro produtos :                          *
      *                                                             *
      *  Data da ultima alteracao:    04/04/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgce02.
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
           select arqce02 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ce02-chave
                  alternate record key is ce02-chave-1 with duplicates
                  file status is ce02-status.
      *
       data division.
       file section.
      *    
       copy fdce01.lib.
      *    
       copy fdce02.lib.
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
       01 ce02-status                  pic x(02) value "00".
       01 ce02-stat                    pic x(01) value "F".
      *
       01 nome-arq-ce02.
          02 ce02-dir                  pic x(03) value "CE2".
          02 filler                    pic x(01) value "\".
          02 ce02-nome                 pic x(08) value "ARQCE02A".
          02 filler                    pic x(01) value ".".
          02 ce02-ext                  pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGCE02".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(50) value spaces.
       01 limpa-aux                    pic x(10) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 grupo                     pic 9(05) value 0.
          02 dgrupo                    pic x(40) value spaces.
          02 produto                   pic 9(05) value 0.
          02 descricao                 pic x(40) value spaces.
          02 descricao-a               pic x(40) value spaces.
          02 estoque-min               pic -zzz.zz9 value 0.
          02 estoque-max               pic -zzz.zz9 value 0.
          02 estoque-med               pic -zzz.zz9 value 0.
          02 estoque-ofic              pic -zzz.zz9 value 0.
          02 estoque-real              pic -zzz.zz9 value 0.
          02 unidade                   pic x(10) value spaces.
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
          02 line 10 column 06 foreground-color 06 background-color 04
             value "Grupo.........:".
          02 line 11 column 06 foreground-color 06 background-color 04
             value "Produto.......:".
          02 line 12 column 06 foreground-color 06 background-color 04
             value "Descricao.....:".
          02 line 14 column 06 foreground-color 06 background-color 04
             value "Estoque Min...:".
          02 line 15 column 06 foreground-color 06 background-color 04
             value "Estoque Max...:".
          02 line 16 column 06 foreground-color 06 background-color 04
             value "Estoque Med...:".
          02 line 17 column 06 foreground-color 06 background-color 04
             value "Estoque Real..:".
      *
       01 tela-02.
          02 line 19 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 19 column 05 foreground-color 07 background-color 02
             highlight value "F1".
          02 line 19 column 07 foreground-color 01 background-color 02
             value "-Help".
          02 line 19 column 15 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 19 column 17 foreground-color 01 background-color 02
             value "-Consultas".
      *
       01 tela-03.
          02 line 19 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 19 column 06 foreground-color 07 background-color 
             02 highlight value "F1".
          02 line 19 column 08 foreground-color 01 background-color 02
             value "-Help".
          02 line 19 column 16 foreground-color 07 background-color 02
             highlight value "P".
          02 line 19 column 17 foreground-color 01 background-color 02
             value "roduto".
          02 line 19 column 30 foreground-color 07 background-color 02
             highlight value "D".
          02 line 19 column 31 foreground-color 01 background-color 02
             value "escricao".
      *
       01 tela-04.
          02 line 19 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 19 column 06 foreground-color 07 background-color 02 
             highlight value "F2".
          02 line 19 column 08 foreground-color 01 background-color 02
             value "-Alt".
          02 line 19 column 15 foreground-color 07 background-color 02 
             highlight value "F3".
          02 line 19 column 17 foreground-color 01 background-color 02
             value "-Exc".
          02 line 19 column 25 foreground-color 07 background-color 02
             highlight value "Home".
          02 line 19 column 29 foreground-color 01 background-color 02
             value "-Inic".
          02 line 19 column 37 foreground-color 07 background-color 02
             highlight value "End".
          02 line 19 column 40 foreground-color 01 background-color 02
             value "-Fim".
          02 line 19 column 47 foreground-color 07 background-color 02
             highlight value "PgDown".
          02 line 19 column 53 foreground-color 01 background-color 02
             value "-Prox".
          02 line 19 column 60 foreground-color 07 background-color 02
             highlight value "PgUp".
          02 line 19 column 64 foreground-color 01 background-color 02
             value "-Ant".
      *
       01 tela-06.
          02 line 19 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 19 column 05 foreground-color 07 background-color 02
             highlight value "Registro gravado - Tecle <Enter>".
      *
       01 tela-09.
          02 line 09 column 65 foreground-color 07 background-color 04
             highlight value "Inclusao".
      *
       01 tela-10.
          02 line 09 column 65 foreground-color 07 background-color 04
             highlight value "Consulta".
      *
       01 tela-mensagem-cad.
          02 line 19 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 19 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 19 column 05 foreground-color 04 background-color 04
             pic x(68) from spaces.
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
           move 07 to box-lin.
           move 72 to box-col-f.
           move 19 to box-lin-f.
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
           perform sec-inclusao.
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
       rot-move-ce02.
           move grupo to ce02-grupo.
           move produto to ce02-produto.
           move descricao to ce02-chave-1.
           move descricao-a to ce02-descricao-a.
           move estoque-min to ce02-estoque-min.
           move estoque-max to ce02-estoque-max.
           move estoque-med to ce02-estoque-med.
           move estoque-ofic to ce02-estoque-ofic.
           move estoque-real to ce02-estoque-real.
           move param-usr to ce02-usuario.
           move param-data to ce02-data.
      *
       rot-move-campos.
           move ce02-grupo to grupo.
           move ce02-produto to produto.
           move ce02-descricao-a to descricao.
           move ce02-estoque-min to estoque-min.
           move ce02-estoque-max to estoque-max.
           move ce02-estoque-med to estoque-med.
           move ce02-estoque-ofic to estoque-ofic.
           move ce02-estoque-real to estoque-real.
           move ce02-data to dias-corr.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move ce02-usuario to cab-usuario.
      *
       rot-le-ce02.
           move 0 to erro.
           read arqce02 invalid key move 1 to erro.
           if ce02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ce02.
      *
       rot-le-ce02-1.
           move 0 to erro.
           read arqce02 key ce02-chave-1 invalid key move 1 to erro.
           if ce02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ce02-1.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqce02 key is equal ce02-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-ce02
           end-start.
      *
       rot-le-ce02-lock.
           move 0 to erro.
           read arqce02 next. 
           if ce02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ce02-lock
           end-if.
           read arqce02 with kept lock.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqce02 previous at end move 1 to erro.
           if ce02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqce02 next at end move 1 to erro.
           if ce02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-ce02.
           move 0 to erro.
           if ce02-stat = "F"
              open i-o arqce02
              if ce02-status not = "00"
                 move 
                 " Erro de abertura no ARQCE02A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ce02-stat
               end-if
           end-if.
      *
       rot-close-ce02.
           if ce02-stat = "A"
              close arqce02
              move "F" to ce02-stat
           end-if.
      *
       rot-erro-leitura-ce02.
           move " Erro de leitura - ARQCE02A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
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
       rot-le-ce01.
           move 0 to erro.
           read arqce01 invalid key move 1 to erro.
           if ce01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ce01.
      *
       rot-inic-arquivo.
           perform lmp-grupo thru lmp-estoque-real.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-grupo thru lmp-estoque-real.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-display.
           perform rot-move-campos.
           move grupo to ce01-grupo.
           perform rot-le-ce01.
           if erro not = 0
              move "Grupo nao cadastrado" to dgrupo
              move spaces to unidade
           else
              move ce01-descricao-a to dgrupo
              move ce01-unidade to unidade
           end-if.
           perform dsp-grupo thru dsp-estoque-ofic.
           if param-prioridade = 9
              perform dsp-estoque-real
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
           accept resposta at 1868 with auto foreground-color 04
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
       err-grupo-n-c.
           move " Grupo nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-produto-c.
           move " Produto ja cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-estoque.
           move " Valor fora da faixa - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
      *  Sequencia para dar Accept
      *
       acc-grupo.
           accept grupo at 1022 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-unidade.
           accept unidade at 1443 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-produto.
           accept produto at 1122 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-descricao.
           accept descricao at 1222 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-estoque-min.
           accept estoque-min at 1422 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-estoque-max.
           accept estoque-max at 1522 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-estoque-med.
           accept estoque-med at 1622 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-estoque-ofic.
           accept estoque-ofic at 1722 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-estoque-real.
           accept estoque-min at 1742 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-grupo.
           display grupo at 1022 with foreground-color 15 
                   background-color 04.
           display dgrupo at 1028 with foreground-color 15 
                   background-color 04.
      *
       dsp-unidade.
           display unidade at 1443 with foreground-color 15 
                   background-color 04.
      *
       dsp-produto.
           display produto at 1122 with foreground-color 15 
                   background-color 04.
      *
       dsp-descricao.
           display descricao at 1222 with foreground-color 15 
                   background-color 04.
      *
       dsp-estoque-min.
           display estoque-min at 1422 with foreground-color 15 
                   background-color 04.
      *
       dsp-estoque-max.
           display estoque-max at 1522 with foreground-color 15 
                   background-color 04.
      *
       dsp-estoque-med.
           display estoque-med at 1622 with foreground-color 15 
                   background-color 04.
      *
       dsp-estoque-ofic.
           display estoque-ofic at 1722 with foreground-color 15 
                   background-color 04.
      *
       dsp-estoque-real.
           display estoque-real at 1742 with foreground-color 15 
                   background-color 04.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-grupo.
           display limpa at 1022 with foreground-color 15 
                   background-color 04.
      *
       lmp-unidade.
           display limpa-aux at 1443 with foreground-color 15 
                   background-color 04.
      *
       lmp-produto.
           display limpa at 1122 with foreground-color 15 
                   background-color 04.
      *
       lmp-descricao.
           display limpa at 1222 with foreground-color 15 
                   background-color 04.
      *
       lmp-estoque-min.
           display limpa-aux at 1422 with foreground-color 15 
                   background-color 04.
      *
       lmp-estoque-max.
           display limpa-aux at 1522 with foreground-color 15 
                   background-color 04.
      *
       lmp-estoque-med.
           display limpa-aux at 1622 with foreground-color 15 
                   background-color 04.
      *
       lmp-estoque-ofic.
           display limpa-aux at 1722 with foreground-color 15 
                   background-color 04.
      *
       lmp-estoque-real.
           display limpa-aux at 1742 with foreground-color 15 
                   background-color 04.
      *
       sec-inclusao section.
      *
       lab-inc-00.
           display tela-limpa-cad.
           perform rot-open-ce01.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-ce02.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           display tela-09.
           if param-prioridade < 1
              perform sec-consulta
              go to lab-inc-fim
           end-if.
      *
       lab-inc-01.
           display tela-09.
           display tela-02.
           move 0 to grupo.
           perform lmp-grupo thru lmp-unidade.
           perform acc-grupo.
           if escape-key = 1
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform lmp-grupo
              perform sec-consulta
              display tela-09
              go to lab-inc-01
           end-if.
           if grupo = 0
              go to lab-inc-01
           end-if.
           move grupo to ce01-grupo.
           perform rot-le-ce01.
           if erro not = 0
              perform err-grupo-n-c
              go to lab-inc-01
           end-if.
           move ce01-descricao-a to dgrupo.
           move ce01-unidade to unidade.
           perform dsp-grupo thru dsp-unidade.
           display tela-limpa-cad.
      *
       lab-inc-02.
           move 0 to produto.
           perform lmp-produto.
           perform acc-produto.
           if escape-key = 1
              perform lmp-produto
              go to lab-inc-01
           end-if.
           if produto = 0
              go to lab-inc-02
           end-if.
           move grupo to ce02-grupo.
           move produto to ce02-produto.
           perform rot-le-ce02.
           if erro = 0
              perform err-produto-c
              go to lab-inc-02
           end-if.
      *
       lab-inc-03.
           move spaces to descricao.
           perform lmp-descricao.
           perform acc-descricao.
           if escape-key = 1
              perform lmp-descricao
              go to lab-inc-02
           end-if.
           move descricao to txt descricao-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-03
           end-if.
           move txt to descricao ce02-descricao.
           perform rot-le-ce02-1.
           if erro = 0
              perform err-produto-c
              go to lab-inc-03
           end-if.
      *
       lab-inc-04.
           move 0 to estoque-min.
           perform lmp-estoque-min.
           perform acc-estoque-min.
           if escape-key = 1
              perform lmp-estoque-min
              go to lab-inc-03
           end-if.
           perform dsp-estoque-min.
      *
       lab-inc-05.
           move 0 to estoque-max.
           perform lmp-estoque-max.
           perform acc-estoque-max.
           if escape-key = 1
              perform lmp-estoque-max
              go to lab-inc-04
           end-if.
           if estoque-max < estoque-min
              perform err-estoque
              go to lab-inc-05
           end-if.
           perform dsp-estoque-max.
      *
       lab-inc-06.
           move 0 to estoque-med.
           perform lmp-estoque-med.
           perform acc-estoque-med.
           if escape-key = 1
              perform lmp-estoque-med
              go to lab-inc-05
           end-if.
           if estoque-med < estoque-min or
              estoque-med > estoque-max
              perform err-estoque
              go to lab-inc-06
           end-if.
           perform dsp-estoque-med.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-inc-07.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-06
           end-if.
           if resposta = "N"
              perform lmp-produto thru lmp-estoque-real
              display tela-limpa-cad
              go to lab-inc-02
           else
              if resposta not = "S"
                 go to lab-inc-07
              end-if
           end-if.
      *
       lab-inc-08.
           perform rot-move-ce02.
           write reg-ce02 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQCE02A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           display tela-06.
           perform rot-keypress.
           perform lmp-produto thru lmp-estoque-real.
           display tela-02.
           go to lab-inc-02.
      *
       lab-inc-fim.
           perform rot-close-ce02.
           perform rot-close-ce01.
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
                            when kbd2 = 80 or 112
                                 display tela-limpa-cad
                                 perform sec-consulta-produto
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
       sec-consulta-produto section.
      *
       lab-cns-produto-00.
           move 0 to grupo.
           perform lmp-grupo.
           perform acc-grupo.
           if escape-key = 1
              perform lmp-grupo
              go to lab-cns-produto-fim
           end-if.
      *
       lab-cns-produto-00-0.
           move 0 to produto.
           perform lmp-produto.
           perform acc-produto.
           if escape-key = 1
              perform lmp-produto
              go to lab-cns-produto-00
           end-if.
           display tela-04.
           move low-values to ce02-chave.
           move grupo to ce02-grupo.
           move produto to ce02-produto.
      *
       lab-cns-produto-00-a.
           start arqce02 key is not less ce02-chave.
           go to lab-cns-produto-03.
      *
       lab-cns-produto-01.
           perform rot-le-anterior.
           if erro not = 0 or ce02-grupo = grupo and 
              ce02-produto = produto
              perform rot-inic-arquivo
              start arqce02 key is not less ce02-chave
              move 1 to erro
              go to lab-cns-produto-05
           end-if.
           if ce02-chave = high-values
              go to lab-cns-produto-01
           end-if.
           go to lab-cns-produto-04.
      *
       lab-cns-produto-02.
           start arqce02 key is less ce02-chave.
      *
       lab-cns-produto-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ce02
              go to lab-cns-produto-fim
           end-if.
           if ce02-chave = high-values
              perform rot-fim-arquivo
              start arqce02 key is not less ce02-chave
              move 0 to grupo
              move 1 to erro
              go to lab-cns-produto-05
           end-if.
      *
       lab-cns-produto-04.
           perform rot-display.
      *
       lab-cns-produto-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-produto-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-produto-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-produto-03
                    when kbd-aux = 73
                         go to lab-cns-produto-01
                    when kbd-aux = 71
                         move low-values to ce02-chave
                         go to lab-cns-produto-00-a
                    when kbd-aux = 79
                         move high-values to ce02-chave
                         go to lab-cns-produto-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-produto-05
           end-if.
           perform lmp-grupo thru lmp-estoque-real.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-produto-00.
      *
       lab-cns-produto-fim.
           move zeros to campo-kbd.
           perform lmp-grupo thru lmp-estoque-real.
           display tela-limpa.
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
           display tela-04.
           move descricao to txt.
           perform rot-texto.
           move low-values to ce02-chave-1.
           move txt to ce02-descricao in ce02-chave-1.
      *
       lab-cns-descricao-00-a.
           start arqce02 key is not less ce02-chave-1.
           go to lab-cns-descricao-03.
      *
       lab-cns-descricao-01.
           perform rot-le-anterior.
           if erro not = 0 or ce02-grupo = grupo and 
              ce02-produto = produto
              perform rot-inic-arquivo
              start arqce02 key is not less ce02-chave-1
              move 1 to erro
              go to lab-cns-descricao-05
           end-if.
           if ce02-chave = high-values
              go to lab-cns-descricao-01
           end-if.
           go to lab-cns-descricao-04.
      *
       lab-cns-descricao-02.
           start arqce02 key is less ce02-chave-1.
      *
       lab-cns-descricao-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ce02
              go to lab-cns-descricao-fim
           end-if.
           if ce02-chave = high-values
              perform rot-fim-arquivo
              start arqce02 key is not less ce02-chave-1
              move 0 to grupo
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
                         move low-values to ce02-chave-1
                         go to lab-cns-descricao-00-a
                    when kbd-aux = 79
                         move high-values to ce02-chave-1
                         go to lab-cns-descricao-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-descricao-05
           end-if.
           perform lmp-grupo thru lmp-estoque-real.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-descricao-00.
      *
       lab-cns-descricao-fim.
           move zeros to campo-kbd.
           perform lmp-grupo thru lmp-estoque-real.
           display tela-limpa.
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
           perform rot-le-ce02-lock.
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
           delete arqce02 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQCE02A.DAT - Tecle <Enter>
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
           unlock arqce02 record.
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
           perform rot-le-ce02-lock.
           perform rot-display.
      *
       lab-alt-01.
           perform acc-descricao.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           move descricao to txt descricao-a.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-01
           end-if.
           move txt to descricao ce02-descricao.
           perform rot-le-ce02-1.
           if erro = 0 and ce02-grupo not = grupo or
              ce02-produto not = produto
              perform err-produto-c
              go to lab-alt-01
           end-if.
      *
       lab-alt-02.
           perform acc-estoque-min.
           if escape-key = 1
              move descricao-a to descricao
              perform dsp-estoque-min
              go to lab-alt-01
           end-if.
           perform dsp-estoque-min.
      *
       lab-alt-03.
           perform acc-estoque-max.
           if escape-key = 1
              perform dsp-estoque-max
              go to lab-alt-02
           end-if.
           if estoque-max < estoque-min
              perform err-estoque
              go to lab-alt-03
           end-if.
           perform dsp-estoque-max.
      *
       lab-alt-04.
           perform acc-estoque-med.
           if escape-key = 1
              perform lmp-estoque-med
              go to lab-alt-03
           end-if.
           if estoque-med < estoque-min or
              estoque-med > estoque-max
              perform err-estoque
              go to lab-alt-04
           end-if.
           perform dsp-estoque-med.
      *
       lab-alt-05.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-04
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-05
              end-if
           end-if.
           perform rot-move-ce02.
           rewrite reg-ce02 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQCE02A.DAT - Tecle <Ent
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
           unlock arqce02 record.
           display tela-04.
           exit.