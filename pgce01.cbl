      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  PGCE01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao do cadastro grupos :                            *
      *                                                             *
      *  Data da ultima alteracao:    03/04/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgce01.
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
       data division.
       file section.
      *    
       copy fdce01.lib.
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
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGCE01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(45) value spaces.
       01 limpa-aux                    pic x(05) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 grupo                     pic 9(05) value 0.
          02 descricao                 pic x(40) value spaces.
          02 descricao-a               pic x(40) value spaces.
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
          02 line 15 column 06 foreground-color 06 background-color 04
             value "Grupo.........:".
          02 line 16 column 06 foreground-color 06 background-color 04
             value "Descricao.....:".
          02 line 17 column 06 foreground-color 06 background-color 04
             value "Unidade.......:".
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
             highlight value "G".
          02 line 19 column 17 foreground-color 01 background-color 02
             value "rupo".
          02 line 19 column 27 foreground-color 07 background-color 02
             highlight value "D".
          02 line 19 column 28 foreground-color 01 background-color 02
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
          02 line 14 column 65 foreground-color 07 background-color 04
             highlight value "Inclusao".
      *
       01 tela-10.
          02 line 14 column 65 foreground-color 07 background-color 04
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
           move 12 to box-lin.
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
       rot-move-ce01.
           move grupo to ce01-grupo.
           move descricao to ce01-chave-1.
           move descricao-a to ce01-descricao-a.
           move unidade to ce01-unidade.
           move param-usr to ce01-usuario.
           move param-data to ce01-data.
      *
       rot-move-campos.
           move ce01-grupo to grupo.
           move ce01-descricao-a to descricao.
           move ce01-unidade to unidade.
           move ce01-data to dias-corr.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move ce01-usuario to cab-usuario.
      *
       rot-le-ce01.
           move 0 to erro.
           read arqce01 invalid key move 1 to erro.
           if ce01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ce01.
      *
       rot-le-ce01-1.
           move 0 to erro.
           read arqce01 key ce01-chave-1 invalid key move 1 to erro.
           if ce01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ce01-1.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqce01 key is equal ce01-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-ce01
           end-start.
      *
       rot-le-ce01-lock.
           move 0 to erro.
           read arqce01 next. 
           if ce01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ce01-lock
           end-if.
           read arqce01 with kept lock.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqce01 previous at end move 1 to erro.
           if ce01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
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
       rot-inic-arquivo.
           perform lmp-grupo thru lmp-unidade.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-grupo thru lmp-unidade.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-display.
           perform rot-move-campos.
           perform dsp-grupo thru dsp-unidade.
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
       err-grupo-c.
           move " Grupo ja cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
      *  Sequencia para dar Accept
      *
       acc-grupo.
           accept grupo at 1522 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-descricao.
           accept descricao at 1622 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-unidade.
           accept unidade at 1722 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-grupo.
           display grupo at 1522 with foreground-color 15 
                   background-color 04.
      *
       dsp-descricao.
           display descricao at 1622 with foreground-color 15 
                   background-color 04.
      *
       dsp-unidade.
           display unidade at 1722 with foreground-color 15 
                   background-color 04.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-grupo.
           display limpa at 1522 with foreground-color 15 
                   background-color 04.
      *
       lmp-descricao.
           display limpa at 1622 with foreground-color 15 
                   background-color 04.
      *
       lmp-unidade.
           display limpa at 1722 with foreground-color 15 
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
           perform lmp-grupo.
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
           if erro = 0
              perform err-grupo-c
              go to lab-inc-01
           end-if.
           display tela-limpa-cad.
      *
       lab-inc-02.
           move spaces to descricao.
           perform lmp-descricao.
           perform acc-descricao.
           if escape-key = 1
              perform lmp-descricao
              go to lab-inc-01
           end-if.
           move descricao to txt descricao-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-02
           end-if.
           move txt to descricao ce01-descricao.
           perform rot-le-ce01-1.
           if erro = 0
              perform err-grupo-c
              go to lab-inc-02
           end-if.
      *
       lab-inc-03.
           move spaces to unidade.
           perform lmp-unidade.
           perform acc-unidade.
           if escape-key = 1
              perform lmp-unidade
              move descricao-a to descricao
              go to lab-inc-02
           end-if.
           if unidade = spaces
              go to lab-inc-03
           end-if.
      *
       lab-inc-04.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-03
           end-if.
           if resposta = "N"
              perform lmp-grupo thru lmp-unidade
              display tela-02
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-04
              end-if
           end-if.
      *
       lab-inc-07.
           perform rot-move-ce01.
           write reg-ce01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQCE01A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           display tela-06.
           perform rot-keypress.
           perform lmp-grupo thru lmp-unidade.
           display tela-02.
           go to lab-inc-01.
      *
       lab-inc-fim.
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
                            when kbd2 = 71 or 103
                                 display tela-limpa-cad
                                 perform sec-consulta-grupo
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
       sec-consulta-grupo section.
      *
       lab-cns-grupo-00.
           move 0 to grupo.
           perform lmp-grupo.
           perform acc-grupo.
           if escape-key = 1
              perform lmp-grupo
              go to lab-cns-grupo-fim
           end-if.
           display tela-04.
           move low-values to ce01-chave.
           move grupo to ce01-grupo.
      *
       lab-cns-grupo-00-a.
           start arqce01 key is not less ce01-chave.
           go to lab-cns-grupo-03.
      *
       lab-cns-grupo-01.
           perform rot-le-anterior.
           if erro not = 0 or ce01-grupo = grupo
              perform rot-inic-arquivo
              start arqce01 key is not less ce01-chave
              move 1 to erro
              go to lab-cns-grupo-05
           end-if.
           if ce01-chave = high-values
              go to lab-cns-grupo-01
           end-if.
           go to lab-cns-grupo-04.
      *
       lab-cns-grupo-02.
           start arqce01 key is less ce01-chave.
      *
       lab-cns-grupo-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ce01
              go to lab-cns-grupo-fim
           end-if.
           if ce01-chave = high-values
              perform rot-fim-arquivo
              start arqce01 key is not less ce01-chave
              move 0 to grupo
              move 1 to erro
              go to lab-cns-grupo-05
           end-if.
      *
       lab-cns-grupo-04.
           perform rot-display.
      *
       lab-cns-grupo-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-grupo-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-grupo-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-grupo-03
                    when kbd-aux = 73
                         go to lab-cns-grupo-01
                    when kbd-aux = 71
                         move low-values to ce01-chave
                         go to lab-cns-grupo-00-a
                    when kbd-aux = 79
                         move high-values to ce01-chave
                         go to lab-cns-grupo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-grupo-05
           end-if.
           perform lmp-grupo thru lmp-unidade.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-grupo-00.
      *
       lab-cns-grupo-fim.
           move zeros to campo-kbd.
           perform lmp-grupo thru lmp-unidade.
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
           move low-values to ce01-chave-1.
           move txt to ce01-descricao in ce01-chave-1.
      *
       lab-cns-descricao-00-a.
           start arqce01 key is not less ce01-chave-1.
           go to lab-cns-descricao-03.
      *
       lab-cns-descricao-01.
           perform rot-le-anterior.
           if erro not = 0 or ce01-grupo = grupo
              perform rot-inic-arquivo
              start arqce01 key is not less ce01-chave-1
              move 1 to erro
              go to lab-cns-descricao-05
           end-if.
           if ce01-chave = high-values
              go to lab-cns-descricao-01
           end-if.
           go to lab-cns-descricao-04.
      *
       lab-cns-descricao-02.
           start arqce01 key is less ce01-chave-1.
      *
       lab-cns-descricao-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ce01
              go to lab-cns-descricao-fim
           end-if.
           if ce01-chave = high-values
              perform rot-fim-arquivo
              start arqce01 key is not less ce01-chave-1
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
                         move low-values to ce01-chave-1
                         go to lab-cns-descricao-00-a
                    when kbd-aux = 79
                         move high-values to ce01-chave-1
                         go to lab-cns-descricao-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-descricao-05
           end-if.
           perform lmp-grupo thru lmp-unidade.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-descricao-00.
      *
       lab-cns-descricao-fim.
           move zeros to campo-kbd.
           perform lmp-grupo thru lmp-unidade.
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
           perform rot-le-ce01-lock.
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
           delete arqce01 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQCE01A.DAT - Tecle <Enter>
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
           unlock arqce01 record.
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
           perform rot-le-ce01-lock.
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
           move txt to descricao ce01-descricao.
           perform rot-le-ce01-1.
           if erro = 0 and ce01-grupo not = grupo
              perform err-grupo-c
              move descricao-a to descricao
              go to lab-alt-01
           end-if.
      *
       lab-alt-02.
           perform acc-unidade.
           if escape-key = 1
              move descricao-a to descricao
              go to lab-alt-01
           end-if.
           if unidade = spaces
              go to lab-alt-02
           end-if.
      *
       lab-alt-03.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-02
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-02
              end-if
           end-if.
           perform rot-move-ce01.
           rewrite reg-ce01 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQCE01A.DAT - Tecle <Ent
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
           unlock arqce01 record.
           display tela-04.
           exit.