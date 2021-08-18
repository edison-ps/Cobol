      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  ROTOBS01     *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Cadastro de Observacoes :                                  * 
      *                                                             *
      *  Data da ultima alteracao:    19/06/95     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. rotobs01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqobs01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is obs01-chave
                  file status is obs01-status.
      *
       data division.
       file section.
      *     
       copy fdobs01.lib.
      *     
       working-storage section.
      *
       01 obs01-status                 pic x(02) value "00".
       01 obs01-stat                   pic x(01) value "F".
      *
       01 nome-arq-obs01.
          02 obs01-dir                 pic x(03) value "USR".
          02 filler                    pic x(01) value "\".
          02 obs01-nome                pic x(08) value "ARQOBS01".
          02 filler                    pic x(01) value ".".
          02 obs01-ext                 pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "ROTOBS01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 campos.
          02 obs-01                    pic x(50) value spaces.
          02 obs-02                    pic x(50) value spaces.
          02 obs-03                    pic x(50) value spaces.
          02 obs-04                    pic x(50) value spaces.
          02 obs-05                    pic x(50) value spaces.
          02 obs-06                    pic x(50) value spaces.
          02 obs-07                    pic x(50) value spaces.
          02 obs-08                    pic x(50) value spaces.
          02 obs-09                    pic x(50) value spaces.
          02 obs-10                    pic x(50) value spaces.
          02 obs-11                    pic x(50) value spaces.
          02 obs-12                    pic x(50) value spaces.
          02 obs-13                    pic x(50) value spaces.
          02 obs-14                    pic x(50) value spaces.
          02 obs-15                    pic x(50) value spaces.
      *
       01 linha                        pic 9(04) comp-5 value 0.
       01 coluna                       pic 9(04) comp-5 value 0.
       01 limpa                        pic x(50) value spaces.
       01 mens-erro                    pic x(59) value spaces.
       01 limpa-aux                    pic x(40) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 tamanho                      pic 9(04) comp-5 value 59.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
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
       01 campo-rotina.
          02 obs-arquivo               pic 9(02).
          02 obs-codigo                pic x(10).
      *
       screen section.
      *
       01 tela-01.
          02 line 07 column 49 foreground-color 06 background-color 01
             highlight value "Observacoes".
      *
       01 tela-02.
          02 line 08 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-01.
          02 line 09 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-02.
          02 line 10 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-03.
          02 line 11 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-04.
          02 line 12 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-05.
          02 line 13 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-06.
          02 line 14 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-07.
          02 line 15 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-08.
          02 line 16 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-09.
          02 line 17 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-10.
          02 line 18 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-11.
          02 line 19 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-12.
          02 line 20 column 09 foreground-color 06 background-color 01
             highlight prompt auto pic x(50) using obs-13.
      *
       01 tela-03.
          02 line 08 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
          02 line 09 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
          02 line 10 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
          02 line 11 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
          02 line 12 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
          02 line 13 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
          02 line 14 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
          02 line 15 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
          02 line 16 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
          02 line 17 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
          02 line 18 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
          02 line 19 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
          02 line 20 column 09 foreground-color 06 background-color 01
             highlight pic x(50) using limpa.
      *
       01 tela-mensagem-cad.
          02 line 22 column 08 foreground-color 07 background-color 01
             highlight pic x(52) from mensagem.
      *
       01 tela-erro-cad.
          02 line 22 column 08 beep reverse-video pic x(52) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 22 column 08 foreground-color 01 background-color 01
             pic x(52) from spaces.
      *
       copy scrgen.lib.
      *
       procedure division using param-menu campo-rotina.
      *
       lab-00.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-save-buffer.
           display tela-cabec.
           move 06 to box-col.
           move 05 to box-lin.
           move 59 to box-col-f.
           move 22 to box-lin-f.
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
           perform sec-observacao.
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
       rot-move-obs01.
           move obs-arquivo to obs01-arquivo.
           move obs-codigo to obs01-codigo.
           move obs-01 to obs01-obs (01).
           move obs-02 to obs01-obs (02).
           move obs-03 to obs01-obs (03).
           move obs-04 to obs01-obs (04).
           move obs-05 to obs01-obs (05).
           move obs-06 to obs01-obs (06).
           move obs-07 to obs01-obs (07).
           move obs-08 to obs01-obs (08).
           move obs-09 to obs01-obs (09).
           move obs-10 to obs01-obs (10).
           move obs-11 to obs01-obs (11).
           move obs-12 to obs01-obs (12).
           move obs-13 to obs01-obs (13).
           move param-usr to obs01-usuario.
           move param-data to obs01-data.
      *
       rot-move-campos.
           move obs01-arquivo to obs-arquivo.
           move obs01-codigo to obs-codigo.
           move obs01-obs (01) to obs-01.
           move obs01-obs (02) to obs-02.
           move obs01-obs (03) to obs-03.
           move obs01-obs (04) to obs-04.
           move obs01-obs (05) to obs-05.
           move obs01-obs (06) to obs-06.
           move obs01-obs (07) to obs-07.
           move obs01-obs (08) to obs-08.
           move obs01-obs (09) to obs-09.
           move obs01-obs (10) to obs-10.
           move obs01-obs (11) to obs-11.
           move obs01-obs (12) to obs-12.
           move obs01-obs (13) to obs-13.
           move obs01-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move data-disp to cab-data.
           move obs01-usuario to cab-usuario.
      *
       rot-display.
           perform rot-move-campos.
           display tela-02.
           if param-prioridade = 9 and erro = 0
              move cab-usr to mensagem
              display tela-mensagem
           end-if.

      *
       rot-le-obs01.
           move 0 to erro.
           read arqobs01 invalid key move 1 to erro.
           if obs01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-obs01.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqobs01 key is equal obs01-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-obs01
           end-start.
      *
       rot-le-obs01-lock.
           move 0 to erro.
           read arqobs01 next. 
           if obs01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-obs01-lock
           end-if.
           read arqobs01 with kept lock.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqobs01 previous at end move 1 to erro.
           if obs01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqobs01 next at end move 1 to erro.
           if obs01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-obs01.
           move 0 to erro.
           if obs01-stat = "F"
              open i-o arqobs01
              if obs01-status not = "00"
                 move 
                 " Erro de abertura no ARQOBS01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
                 move zeros to reg-obs01
                 move high-values to obs01-controle
                 write reg-obs01-1
               else
                  move "A" to obs01-stat
               end-if
           end-if.
      *
       rot-close-obs01.
           if obs01-stat = "A"
              close arqobs01
              move "F" to obs01-stat
           end-if.
      *
       rot-erro-leitura-obs01.
           move " Erro de leitura - ARQOBS01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-inic-arquivo.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-grava-obs01.
           move 0 to erro.
           write reg-obs01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQOBS01.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
           end-write.
      *
       rot-regrava-obs01.
           move 0 to erro.
           rewrite reg-obs01 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQOBS01.DAT - Tecle <Ent
      -            "er>" to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
           end-rewrite.
      *
       rot-exc-obs01.
           move 0 to erro.
           delete arqobs01 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQOBS01.DAT - Tecle <Enter>
      -           " " to mensagem
                  display tela-erro
                  perform rot-keypress
                  display tela-limpa
           end-delete.
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
       display-erro-usr.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
       sec-observacao section.
      *
       lab-obs-00.
           display tela-limpa-cad.
           perform rot-open-obs01.
           if erro not = 0
              go to lab-obs-fim
           end-if.
      *     if param-prioridade < 1
      *        go to lab-obs-fim
      *     end-if.
      *
       lab-obs-01.
           move obs-arquivo to obs01-arquivo.
           move obs-codigo to obs01-codigo.
           perform rot-le-obs01.
           if erro = 0
              perform rot-ponteiro
              perform rot-le-obs01-lock
              go to lab-obs-02
           end-if.
           move spaces to campos.
           perform rot-move-obs01.
           perform rot-grava-obs01.
           if erro not = 0
              go to lab-obs-fim
           end-if.
           perform rot-ponteiro.
           perform rot-le-obs01-lock.
           move 1 to erro.
      *
       lab-obs-02.
           perform rot-display.
           accept tela-02
                  on escape accept escape-key from escape
                     evaluate true
                              when escape-key = 1
                                   if obs01-observacao = spaces
                                      perform rot-exc-obs01
                                   end-if
                                   go to lab-obs-fim
                              when escape-key = 4 and erro = 0
                                   perform sec-exc-obs
                                   go to lab-obs-fim
                              when escape-key = 5
                                   move spaces to campos
                                   go to lab-obs-02
                              when other
                                   go to lab-obs-02
                     end-evaluate.
      *
       lab-obs-03.
           move "Cadastrar/Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-obs-02
           end-if.
           if resposta = "N"
              go to lab-obs-02
           else
              if resposta not = "S"
                 go to lab-obs-03
              end-if
           end-if.
           perform rot-move-obs01.
           perform rot-regrava-obs01.
           if erro = 0
              move "Registro gravado - Tecle <Enter>" to mensagem
              display tela-mensagem-cad
              perform rot-keypress
           else
              go to lab-obs-fim
           end-if.
           display tela-limpa-cad.
           go to lab-obs-01.
      *
       lab-obs-fim.
           unlock arqobs01 record.
           perform rot-close-obs01.
           exit.
      *
       sec-exc-obs section.
      *
       lab-exc-obs-00-0.
           display tela-limpa-cad.
           if param-prioridade < 7
              perform display-erro-usr
              go to lab-exc-obs-fim
           end-if.
      *
       lab-exc-obs-00.
           move "Excluir (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-exc-obs-01.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-exc-obs-fim
           end-if.
           if resposta = "N"
              go to lab-exc-obs-fim
           else
              if resposta not = "S"
                 go to lab-exc-obs-01
              end-if
           end-if.
           perform rot-exc-obs01.
           if erro not = 0
              go to lab-exc-obs-fim
           end-if.
           move "Registro excluido - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       lab-exc-obs-fim.
           display tela-limpa-cad.
           exit.
      *